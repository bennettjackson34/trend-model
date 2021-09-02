const lead = (arr = [], n = 1, fill = null) => {
	return arr.slice(n).concat(new Array(n).fill(fill));
};

const lag = (arr = [], n = 1, fill = null) => {
	return new Array(n).fill(fill).concat(arr.slice(n - 1, arr.length - 1));
};

const range = (start, end) => {
	return Array(end - start + 1)
		.fill()
		.map((_, idx) => start + idx);
};

function getATR(data = [], params) {
	let tr = [];

	// convert object to array
	data = Array.isArray(data) ? data : Object.values(data);

	data.forEach(function (point, i) {
		const point_tr = { x: point[params.key.x], y: null };

		if (i) {
			// get true range of point
			const true_high = Math.max(point[params.key.high], data[i - 1][params.key.close]);
			const true_low = Math.min(point[params.key.low], data[i - 1][params.key.close]);
			point_tr.y = true_high - true_low;
		} else {
			point_tr.y = point[params.key.high] - point[params.key.low];
		}

		// push point to array
		tr.push(point_tr);
	});

	atr = getSMMA(tr, { n: params.n, key: { x: "x", source: "y" } });
	return atr;
}

function getSMMA(data = [{ x, y }], params = { n, key: { source } }) {
	const n = params.n.value;
	const key = params.key.source;
	let smma = [];

	// convert object to array
	data = Array.isArray(data) ? data : Object.values(data);

	if (data.length >= n) {
		let initialValue = 0;

		data.forEach(function (point, i) {
			let point_smma = { x: point[params.key.x], y: null };

			if (i >= n) {
				point_smma.y = (smma[smma.length - 1].y * (n - 1) + point[key]) / n;
			} else if (i == n - 1) {
				// first entry is a simple average
				point_smma.y =
					data.slice(0, n).reduce(function (total, currentValue) {
						return total + currentValue[key];
					}, initialValue) / n;
			}

			smma.push(point_smma);
		});
	}
	return smma;
}

function getZigZag(data, params = { key: { x, open, high, low, close } }, dateindex = null, asset = null, save = false) {
	const hi = data.map((i) => i[params.key.high]);
	const prev_hi = lag(hi, 1);
	const next_hi = lead(hi, 1);
	const candidate_peak_highs = hi.reduce((res, point, i) => {
		if (point >= prev_hi[i] && point > next_hi[i]) res.push(i);
		return res;
	}, []);

	const lo = data.map((i) => i[params.key.low]);
	const prev_lo = lag(lo, 1);
	const next_lo = lead(lo, 1);
	const candidate_peak_lows = lo.reduce((res, point, i) => {
		if (point <= prev_lo[i] && point < next_lo[i]) res.push(i);
		return res;
	}, []);

	const all_candidate_peaks = candidate_peak_highs.concat(candidate_peak_lows).reduce((res, point) => {
		if (!res.includes(point)) res.push(point);
		return res;
	}, []);

	// sort peak days in ascending order
	all_candidate_peaks.sort((a, b) => a - b);

	let res = { series: new Array(hi.length).fill(NaN), peaks: { Highs: [], Lows: [] } };
	var env = { last_peak: "", replace: false };

	const getPeakHigh = (data, day, res) => {
		if (!env.replace) {
			// if we didn't overwrite a peak already
			const hi = data.map((i) => i[params.key.high]);
			const lo = data.map((i) => i[params.key.low]);

			const upper_threshold = hi[day];

			// check left side of peak
			const left_lower_threshold = Math.max(...lo.slice(day - 1, day + 1));
			const left_range = range(0, day - 1);

			// get the first day to confirm the left side of the peak
			const left_lower_confirm_day = Math.max(...left_range.filter((i) => lo[i] < left_lower_threshold));
			const left_upper_confirm_day = Math.max(...left_range.filter((i) => hi[i] < upper_threshold));
			const left_confirm_day = Math.min(left_upper_confirm_day, left_lower_confirm_day);

			// get the first day to violate the left side of the peak
			const left_negate_day = Math.max(...left_range.filter((i) => hi[i] > upper_threshold));

			// check if confirm date is closer than negate date
			// left = true | false
			const left = left_confirm_day ? (left_negate_day ? left_confirm_day >= left_negate_day : true) : false;

			// check the right side of peak
			const right_lower_threshold = Math.max(...lo.slice(day, day + 2));
			const right_range = range(day + 1, data.length - 1);

			// get the first day to confirm the right side of the peak (only search up until the next candidate peak)
			const right_lower_confirm_day = Math.min(...right_range.filter((i) => lo[i] < right_lower_threshold));
			const right_upper_confirm_day = Math.min(...right_range.filter((i) => hi[i] < upper_threshold));
			const right_confirm_day = Math.max(right_upper_confirm_day, right_lower_confirm_day);

			// get the first day to violate the right side of the peak
			const right_negate_day = Math.min(...right_range.filter((i) => hi[i] > upper_threshold));

			// check if confirm date is closer than negate date
			// right = true | false
			const right = right_confirm_day ? (right_negate_day ? right_confirm_day <= right_negate_day : true) : false;

			//console.log(right_range);
			if (left && right) {
				if (env.last_peak == "High") {
					// last peak was a high, need to consider if this should overwrite the previous one
					// overwrite if this new peak high is > previous
					env.replace = hi[day] > hi[res.peaks.Highs[res.peaks.Highs.length - 1]];

					if (env.replace) {
						// clear out ZZ level on the series and replace with new
						res.series[res.peaks.Highs[res.peaks.Highs.length - 1]] = NaN;
						res.series[day] = hi[day];

						// replace peak low with today
						res.peaks.Highs[res.peaks.Highs.length - 1] = day;
					}
				} else {
					res.peaks.Highs.push(day);
					res.series[day] = hi[day];
					env.last_peak = "High";
				}
			}
		}
		return res;
	};

	const getPeakLow = (data, day, res) => {
		if (!env.replace) {
			// if we didn't overwrite a peak already
			const hi = data.map((i) => i[params.key.high]);
			const lo = data.map((i) => i[params.key.low]);

			const lower_threshold = lo[day];

			// check left side of peak
			const left_upper_threshold = Math.min(...hi.slice(day - 1, day + 1));
			const left_range = range(0, day - 1);

			// get the first day to confirm the left side of the peak
			const left_upper_confirm_day = Math.max(...left_range.filter((i) => hi[i] > left_upper_threshold));
			const left_lower_confirm_day = Math.max(...left_range.filter((i) => lo[i] > lower_threshold));
			const left_confirm_day = Math.min(left_upper_confirm_day, left_lower_confirm_day);

			// get the first day to violate the left side of the peak
			const left_negate_day = Math.max(...left_range.filter((i) => lo[i] < lower_threshold));

			// check if confirm date is closer than negate date
			// left = true | false
			const left = left_confirm_day ? (left_negate_day ? left_confirm_day >= left_negate_day : true) : false;

			// check the right side of peak
			const right_upper_threshold = Math.min(...hi.slice(day, day + 2));
			const right_range = range(day + 1, data.length - 1);

			// get the first day to confirm the right side of the peak (only search up until the next candidate peak)
			const right_upper_confirm_day = Math.min(...right_range.filter((i) => hi[i] > right_upper_threshold));
			const right_lower_confirm_day = Math.min(...right_range.filter((i) => lo[i] > lower_threshold));
			const right_confirm_day = Math.max(right_upper_confirm_day, right_lower_confirm_day);

			// get the first day to violate the right side of the peak
			const right_negate_day = Math.min(...right_range.filter((i) => lo[i] < lower_threshold));

			// check if confirm date is closer than negate date
			// right = true | false
			const right = right_confirm_day ? (right_negate_day ? right_confirm_day <= right_negate_day : true) : false;

			//console.log(right_range);
			if (left && right) {
				if (env.last_peak == "Low") {
					// last peak was a low, need to consider if this should overwrite the previous one
					// overwrite if this new peak low is  less than the previous
					env.replace = lo[day] < lo[res.peaks.Lows[res.peaks.Lows.length - 1]];

					if (env.replace) {
						// clear out ZZ level on the series and replace with new
						res.series[res.peaks.Lows[res.peaks.Lows.length - 1]] = NaN;
						res.series[day] = lo[day];

						// replace peak low with today
						res.peaks.Lows[res.peaks.Lows.length - 1] = day;
					}
				} else {
					res.peaks.Lows.push(day);
					res.series[day] = lo[day];
					env.last_peak = "Low";
				}
			}
		}
		return res;
	};

	all_candidate_peaks.forEach((day) => {
		env.replace = false;

		if (env.last_peak == "Low") {
			res = getPeakLow(data, day, res);
			res = getPeakHigh(data, day, res);
		} else {
			res = getPeakHigh(data, day, res);
			res = getPeakLow(data, day, res);
		}
		//return;
	});

	if (save) {
		null;
	}

	return res;
}

function getConfirmationDates(x, peaks) {
	const res = {};
	const allPeaks = peaks.Highs.concat(peaks.Lows).sort((a, b) => a - b);

	allPeaks.forEach((day) => {
		const type = peaks.Highs.includes(day) ? -1 : 1;
		const extreme_pos = type == 1 ? "High" : "Low";
		const extreme_neg = type == 1 ? "Low" : "High";

		const confirm_range = range(day + 1, x.length - 1);
		const threshold = type * Math.min(...x.slice(day, day + 2).map((i) => type * i[extreme_pos]));
		const confirm_day = Math.min(...confirm_range.filter((i) => type * x[i][extreme_pos] > type * threshold));

		res[day] = confirm_day;
	});
	return res;
}

function getChannels(x, rng, peaks, trend) {
	const data = x.slice(rng[0], rng[rng.length - 1] + 1);
	const extreme_pos = trend == 1 ? "High" : "Low";
	const extreme_neg = trend == 1 ? "Low" : "High";
	let trend_extreme_day = 0;
	let trend_extreme = data[trend_extreme_day][extreme_neg];

	const res = { days: [], size: [], current_rotation_size: 0, current_rotation_day: 0 };

	for (let day = 1; day < rng.length; day++) {
		if (trend * data[day][extreme_pos] > trend * trend_extreme) {
			// we've made a new trend extreme
			const new_trend_extreme = data[day][extreme_pos];
			// look for the peak that separates the drive to new extremes
			const candidate_peaks = peaks.filter((peak_day) => peak_day > rng[trend_extreme_day] && peak_day <= rng[day]);

			if (candidate_peaks.length) {
				const extreme_peak = trend * Math.min(...candidate_peaks.map((peak_day) => trend * x[peak_day][extreme_neg]));
				const channel_day = Math.min(...candidate_peaks.filter((peak_day) => x[peak_day][extreme_neg] == extreme_peak));
				const channel_size = Math.abs(trend_extreme - x[channel_day][extreme_neg]);
				res.days.push(channel_day);
				res.size.push(channel_size);
			}

			trend_extreme = new_trend_extreme;
			trend_extreme_day = day;
		}
	}

	// find the size of the current rotation
	const current_rotation_range = range(rng[trend_extreme_day], rng[rng.length - 1]);
	const current_rotation_peaks = peaks.filter((peak_day) => current_rotation_range.includes(peak_day));

	if (current_rotation_peaks.length) {
		const extreme_peak = trend * Math.min(...current_rotation_peaks.map((peak_day) => trend * x[peak_day][extreme_neg]));
		const current_peak_day = Math.min(...current_rotation_peaks.filter((peak_day) => x[peak_day][extreme_neg] == extreme_peak));
		const current_rotation_size = Math.abs(extreme_peak - trend * Math.max(...x.slice(current_peak_day).map((i) => trend * i[extreme_pos])));
		res.current_rotation_day = current_peak_day;
		res.current_rotation_size = current_rotation_size;
	}

	return res;
}

module.exports = { getSMMA, getATR, getZigZag, getConfirmationDates, getChannels };
