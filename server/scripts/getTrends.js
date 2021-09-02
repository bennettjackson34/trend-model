getTrends = function (data = {}) {
	console.log("Getting Trends...");

	let fs = require("fs");
	const indicators = require("./studies.js");
	const { range } = require("./helpers.js");

	// let data = {};
	const results = {};

	// assets.map((asset) => {
	// 	data[asset] = test_data;
	// });

	const start_buffer = 251;

	Object.entries(data).map((_) => {
		// loop through each asset
		const asset = _[0];
		let x = _[1];
		console.log("Getting trends for " + asset);

		x = x.map((i) => {
			const res = (({ Date, Open, High, Low, Close }) => ({ Date, Open, High, Low, Close }))(i);

			// replace all null values with the closing price
			Object.keys(res).map((source) => {
				res[source] = res[source] ? res[source] : res["Close"];
			});

			return res;
		});

		// array of dates
		const dateindex = x.map((i) => {
			return i["Date"];
		});

		// list of holidays
		const holidays_list = ["Christmas", "New Years"];
		const holidays = [];

		// length of data
		const n = x.length;

		// Studies
		const moving_avg = indicators.getSMMA(x, { n: { value: 20 }, key: { x: "Date", source: "Close" } });
		const ATR = indicators.getATR(x, { n: { value: 20 }, key: { x: "Date", open: "Open", high: "High", low: "Low", close: "Close" } });
		const ZZs = indicators.getZigZag(x, { key: { x: "Date", open: "Open", high: "High", low: "Low", close: "Close" } }, dateindex, asset, false);
		const ZZpeaks = ZZs.peaks;
		const confirm_dates = indicators.getConfirmationDates(x, ZZpeaks);

		//variables
		//start at first ZZ
		const start = Math.min(ZZpeaks.Highs.filter((i) => i > start_buffer - 1)[0], ZZpeaks.Lows.filter((i) => i > start_buffer - 1)[0]);
		let trend_start_day = start_buffer;
		let reversal_date = trend_start_day;
		let trend_defining_day = start;
		let pvf_peak_day = start;
		let pvf = 0;
		let one_time_shift_rule = false;
		let trend_defining_level = null;
		let trend_extreme_day = trend_start_day;
		let current_short_side = null;
		let trend = null;
		let shift = false;
		let double_battle_line = false;
		let battle_line_1_day = 0;
		let battle_line_2_day = 0;
		let new_extreme_battle_line = false;
		const MA_threshold = 1.5;
		let pattern_override = false;
		let MA_override = false;

		// Filter parameters
		let bad_trend_count = 0;
		const bad_trend_threshold = 2;
		let bad_trend = true;
		let bad_trend_filter = false;
		let bad_trend_probe = false;
		let bad_trend_reference_day = trend_start_day;

		let use_extended_impulse_filter = true;
		const extended_impulse_on_threshold = 5;
		const extended_impulse_off_threshold = 1.5;
		let extended_impulse_filter = false;
		let extended_impulse_start_day = 0;
		let extended_impulse_channel_count = 0;

		let use_large_rotation_filter = true;
		// How big the long side of the rotation needs to be
		const large_rotation_filter_threshold_1 = 5;
		// How far apart the subsequent peak needs to be from the rotation extreme (at least)
		const large_rotation_filter_threshold_2 = 2.5;

		let res = new Array(x.length).fill().map((_, day) => {
			return {
				Date: dateindex[day],
				Notes: "",
				Trend: null,
				"Pattern Level": null,
				"Trend Defining Level": null,
				"Pattern Level Day": null,
				"Moving Average": moving_avg[day].y,
				ATR: ATR[day].y,
				Open: x[day].Open,
				High: x[day].High,
				Low: x[day].Low,
				Close: x[day].Close,
			};
		});

		if (ZZpeaks.Highs.includes(start)) {
			// first ZZ is a peak high, start in a bear trend
			trend = -1;
			trend_defining_level = x[trend_defining_day].High;
			pvf = Math.abs(x[trend_start_day].Low - trend_defining_level);
			pvf_peak = x[pvf_peak_day].High;
		} else {
			// first ZZ is a peak low, start in a bull trend
			trend = 1;
			trend_defining_level = x[trend_defining_day].Low;
			pvf = Math.abs(x[trend_start_day].High - trend_defining_level);
			pvf_peak = x[pvf_peak_day].Low;
		}

		for (let day = start; day < n; day++) {
			let extreme_pos = trend == 1 ? "High" : "Low";
			let extreme_neg = trend == 1 ? "Low" : "High";
			let ZZ_key = trend == 1 ? "Lows" : "Highs";
			let day_range = x[day].High - x[day].Low;
			let bad_trend_shift = false;
			let new_extreme_battle_line = false;

			// check for a full day above/below both the pattern level and moving average
			const full_day_pattern = trend * x[day][extreme_pos] < trend * trend_defining_level;
			const full_day_MA = trend * x[day][extreme_pos] < (moving_avg[day].y ? trend * moving_avg[day].y : -Infinity);

			// decide whether to use the pattern, moving average level, or both
			const full_day = pattern_override ? full_day_pattern : MA_override ? full_day_MA : full_day_pattern && full_day_MA;

			// calculate the quartile for the day
			const quartile = trend * x[day].Close < trend * x[day][extreme_pos] - day_range / 4;

			// reversal requires a full day AND a close in the correct quartile(s)
			const reversal = full_day && quartile;

			if (full_day && !quartile) {
				res[day].Notes += (res[day].Notes ? "; " : "") + "Quartile rule prevented reversal";
			}

			if (reversal && holidays.includes(day)) {
				res[day].Notes += (res[day].Notes ? "; " : "") + "Holiday prevented reversal";
				reversal < -false;
			}

			if (reversal) {
				res[day].Notes += (res[day].Notes ? "; " : "") + (trend == -1 ? "Bullish" : "Bearish") + " reversal";

				one_time_shift_rule = false;
				double_battle_line = false;
				shift = false;
				extended_impulse_filter = false;
				extended_impulse_channel_count = 0;
				MA_override = false;
				pattern_override = false;

				if (bad_trend) {
					// if previous trend has not had follow through
					// toggle poor trend filter
					bad_trend_filter = !bad_trend_filter;
					// increment poor trend count
					bad_trend_count += 1;
				} else {
					// turn poor trend filter off
					bad_trend_filter = false;
					// reset poor trend count
					bad_trend_count = 0;
				}

				const new_trend = -trend;

				// reset pos/neg trend exteme keys
				extreme_pos = new_trend == 1 ? "High" : "Low";
				extreme_neg = new_trend == 1 ? "Low" : "High";
				ZZ_key = new_trend == 1 ? "Lows" : "Highs";

				// find channels since last trend extreme
				const channel_range = range(trend_extreme_day, day);
				const channels = indicators.getChannels(
					x,
					channel_range,
					ZZpeaks[ZZ_key].filter((peak_day) => peak_day > trend_extreme_day && peak_day < day),
					new_trend
				);

				if (channels.days.length > 1) {
					// multiple channels
					if (channels.current_rotation_size > channels.size[channels.size.length - 1]) {
						// current rotation is larget than the most recent channel
						trend_defining_day = channels.current_rotation_day;
					} else if (channels.size[channels.size.length - 1] > channels.size[channels.size.length - 2]) {
						// most recent channel is the largest
						trend_defining_day = channels.days[channels.days.length - 1];
					} else {
						trend_defining_day = channels.days[channels.days.length - 2];
						double_battle_line = true;
						battle_line_1_day = trend_defining_day;
						battle_line_2_day = channels.days[channels.days.length - 1];
					}

					// if there is at least one channel, the most recent channel sets the pvf
					pvf = channels.size[channels.size.length - 1];
					pvf_peak_day = channels.days[channels.days.length - 1];
				} else if (channels.days.length) {
					if (pvf > channels.size[0]) {
						// previous pvf is larger than the channel
						if (channels.current_rotation_day) {
							// we have a current rotation
							rotation_extreme = channels.current_rotation_day;
							if (channels.current_rotation_size > channels.size[0]) {
								// current rotation is larger than most recent channel
								trend_defining_day = channels.current_rotation_day;
							} else {
								// cdouble battle line rule is already enabled
								trend_defining_day = channels.days[0];
								double_battle_line = true;
								battle_line_1_day = channels.days[0];
								battle_line_2_day = channels.current_rotation_day;
							}
						} else {
							// no current rotation, set trend defining level to previous trend extreme
							// enable double battle line rule
							trend_defining_day = trend_extreme_day;
							double_battle_line = true;
							battle_line_1_day = trend_extreme_day;
							battle_line_2_day = channels.days[0];
						}
					} else {
						// channel is larger than previous pvf
						trend_defining_day = channels.days[0];
					}

					// if there is at least one channel, the most recent channel sets the pvf
					pvf = channels.size[0];
					pvf_peak_day = channels.days[0];
				} else {
					// no fully formed channels since previous trend extreme
					// pvf is left at the previous level unless current rotation is larger
					trend_defining_day = channels.current_rotation_size > pvf ? channels.current_rotation_day : trend_extreme_day;
					pvf_peak_day = trend_extreme_day;
				}

				// last full day spent in the direction of the previous trend
				let bad_reference_day_1 = Math.max(...range(trend_start_day, day).filter((day) => trend * x[day][extreme_pos] > trend * moving_avg[day].y));
				bad_reference_day_1 = Math.abs(bad_reference_day_1) == Infinity ? null : bad_reference_day_1;

				if (!bad_reference_day_1) {
					// default to reversal date if reference day was not found
					bad_trend_reference_day = day;
				} else {
					// first full day spent in the direction of the new trend
					bad_trend_reference_day = Math.min(...range(bad_reference_day_1, day).filter((day) => trend * x[day][extreme_neg] < trend * moving_avg[day].y));
					bad_trend_reference_day = Math.abs(bad_trend_reference_day) == Infinity ? null : bad_trend_reference_day;
				}

				// calculate if there has been sufficient follow through from the reference point
				bad_trend = Math.abs(x[day][extreme_pos] - x[bad_trend_reference_day ? bad_trend_reference_day : day].Close) / ATR[day].y < bad_trend_threshold;

				if (bad_trend_filter && !bad_trend) {
					// poor trend filter is both enabled and removed on the same day
					res[day].Notes += (res[day].Notes ? "; " : "") + "trend " + (trend == 1 ? "support" : "resistance") + " kept wide";
					res[day].Notes += (res[day].Notes ? "; " : "") + "poor trend filter removed (0 days)";
					bad_trend_filter = false;
				}

				if (bad_trend_filter) {
					trend_defining_day = trend_extreme_day;
					// force trend defining level to the pattern
					pattern_override = true;
				}

				// set trend defining level
				trend_defining_level = x[trend_defining_day][extreme_neg];
				rotation_extreme_day = channels.current_rotation_day ? channels.current_rotation_day : pvf_peak_day;
				rotation_extreme = x[rotation_extreme_day][extreme_neg];

				trend = new_trend;
				trend_start_day = trend_extreme_day;

				// max value since prev trend extreme
				trend_extreme_day = range(trend_start_day, day).sort((a, b) => x[b][extreme_pos] - x[a][extreme_pos])[0];
				prev_trend_extreme_day = trend_extreme_day;
				pvf_peak = x[pvf_peak_day][extreme_neg];
				current_short_side = channels.current_rotation_size;
				reversal_date = day;

				if (bad_trend_filter) {
					res[day].Notes +=
						(res[day].Notes ? "; " : "") + "trend " + (trend == 1 ? "support " : "resistance ") + "kept wide (count " + bad_trend_count + ")";
				} else if (bad_trend_count) {
					res[day].Notes += (res[day].Notes ? "; " : "") + "poor trend filter removed (count " + bad_trend_count + ")";
				}

				res[day] = {
					...res[day],
					Trend: trend,
					"Pattern Level": trend_defining_level,
					"Trend Defining Level": trend * Math.min(trend * trend_defining_level, moving_avg[day].y ? trend * moving_avg[day].y : Infinity),
					"Pattern Level Day": dateindex[trend_defining_day],
				};

				// go to next day
				continue;
			}

			if (use_extended_impulse_filter && !extended_impulse_filter) {
				// filter is in use but has not been turned on yet
				if (
					Math.abs((x[day][extreme_pos] - moving_avg[day].y) / ATR[day.y]) > extended_impulse_on_threshold &&
					trend * x[day][extreme_pos] > trend * x[trend_extreme_day][extreme_pos]
				) {
					// day is a new trend extreme and is extended enough from the moving average
					// reset/set variables
					one_time_shift_rule = false;
					double_battle_line = false;
					extended_impulse_filter = true;
					pattern_override = true;
					first_extended_shift = true;
					extended_impulse_start_day = day;
					res[day].Notes += (res[day].Notes ? "; " : "") + "extended impulse filter enabled";
				}
			}

			let trend_range = range(trend_start_day, day);
			const new_trend_extreme = trend * Math.max(...trend_range.map((day) => trend * x[day][extreme_pos]));
			const new_trend_extreme_day = trend_range[trend_range.map((day) => x[day][extreme_pos]).indexOf(new_trend_extreme)];

			if (new_trend_extreme_day > trend_extreme_day) {
				// new trend extreme has been made

				one_time_shift_rule = false;

				// check if there is a peak in between the drive to new extremes
				const candidate_peaks = ZZpeaks[ZZ_key].filter((peak_day) => peak_day > trend_extreme_day && peak_day <= new_trend_extreme_day);
				// make sure peaks have been confirmed
				const confirmed_candidate_peaks = candidate_peaks.filter((peak_day) => confirm_dates[peak_day] <= day);

				if (candidate_peaks.includes(new_trend_extreme_day) && !confirmed_candidate_peaks.includes(new_trend_extreme_day)) {
					// rotation extreme is also the trend extreme
					x[day][extreme_pos] = x[trend_extreme_day][extreme_pos];
				} else {
					// check if the current trend has gone the requires ATR multiple
					bad_trend =
						Math.abs(x[day][extreme_pos] - x[bad_trend_reference_day ? bad_trend_reference_day : reversal_date].Close) / ATR[day].y < bad_trend_threshold;

					if (bad_trend_filter) {
						// bad trend filter is currently enabled
						if (bad_trend) {
							//...and still in a bad trend
							if (Math.min(...range(trend_extreme_day, day).map((day) => trend * x[day][extreme_neg])) < trend * trend_defining_level) {
								bad_trend_probe = true;
							}
						} else {
							// ...but no longer in a bad trend, turn off filter
							res[day].Notes += (res[day].Notes ? "; " : "") + "poor trend filter removed ( " + day - reversal_date + " days)";
							bad_trend_filter = false;
							bad_trend_shift = true;
							pattern_override = false;
						}
					}

					if (confirmed_candidate_peaks.length) {
						const prev_trend_extreme_day = trend_extreme_day;
						const extreme_peak = trend * Math.min(...candidate_peaks.map((peak_day) => trend * x[peak_day][extreme_neg]));

						const new_pvf_peak_day = confirmed_candidate_peaks.filter((peak_day) => x[peak_day][extreme_neg] == extreme_peak)[0];
						const new_pvf_peak = x[new_pvf_peak_day][extreme_neg];
						const new_pvf = Math.abs(x[prev_trend_extreme_day][extreme_pos] - new_pvf_peak);

						if (extended_impulse_filter) extended_impulse_channel_count += 1;

						if (new_pvf >= pvf) {
							// formed a new channel
							if (new_pvf_peak != trend_defining_level) {
								// the current channel extreme is not already trend defining
								shift = true;
								double_battle_line = false;
							}
						} else if (double_battle_line & (battle_line_2_day > trend_defining_day)) {
							if (trend * new_pvf_peak >= trend * pvf_peak) {
								shift = true;
								new_extreme_battle_line = true;
								battle_line_1_day = battle_line_2_day;
								battle_line_2_day = new_pvf_peak_day;
							} else {
								double_battle_line = false;
							}
						} else {
							double_battle_line = true;
							// only shift on a double battle line if we've previously been in a bad trend
							shift = bad_trend_shift;
							battle_line_2_day = new_pvf_peak_day;
							battle_line_1_day = pvf_peak_day;
						}

						pvf = new_pvf;
						pvf_peak_day = new_pvf_peak_day;
						pvf_peak = new_pvf_peak;
						current_short_side = 0;
						rotation_extreme_day = pvf_peak_day;
						rotation_extreme = pvf_peak;
						current_short_side_day = pvf_peak_day;
						current_short_side_peak = pvf_peak;
					} else if (bad_trend_shift & (pvf_peak_day != trend_defining_day)) {
						// PTF has been removed
						// Shift as long as the pvf day is not the same as trend defining
						shift = true;

						current_short_side = 0;
						current_short_side_day = pvf_peak_day;
						current_short_side_peak = pvf_peak;

						rotation_extreme_day = pvf_peak_day;
						rotation_extreme = pvf_peak;
					}

					trend_extreme_day = new_trend_extreme_day;
				}
			} else {
				// no new trend extreme, recalculate the current short side
				// check peaks that have been confirmed since the last trend extreme
				const candidate_peaks = ZZpeaks[ZZ_key].filter((peak_day) => peak_day > trend_extreme_day && confirm_dates[peak_day] <= day);

				if (candidate_peaks.length) {
					// we have at least one confimred peak
					// get the most 'extreme' peak for this rotation
					rotation_extreme = trend * Math.min(...candidate_peaks.map((peak_day) => trend * x[peak_day][extreme_neg]));
					rotation_extreme_day = candidate_peaks[candidate_peaks.map((peak_day) => x[peak_day][extreme_neg]).indexOf(rotation_extreme)];
					const long_side = Math.abs(x[trend_extreme_day][extreme_pos]);

					// find the most extreme value SINCE the rotation extreme
					new_short_side = Math.abs(
						trend * Math.max(...range(rotation_extreme_day + 1, day).map((day) => trend * x[day][extreme_pos])) - rotation_extreme
					);
					current_short_side = Math.max(current_short_side, new_short_side);

					if (new_short_side == current_short_side) {
						// the short side has gotten bigger
						current_short_side_day = rotation_extreme_day;
						current_short_side_peak = rotation_extreme;

						if (current_short_side_peak != trend_defining_level) {
							if (current_short_side >= pvf) {
								// current short side size has exceeded the pvf, shift
								// double battle line rule gets reset
								shift = true;
								double_battle_line = false;
							} else if (double_battle_line && battle_line_2_day > trend_defining_day) {
								// double battle line rule is enabled
								if (trend * current_short_side_peak >= trend * pvf_peak) {
									// shift is in the correction direction (higher for bull trend, lower for bear trend)
									shift = true;
								} else {
									double_battle_line = false;
								}
							}
						}
					}

					if (use_large_rotation_filter && (long_side > large_rotation_filter_threshold_1) & !bad_trend_filter) {
						// long side of the rotation is large enough to start looking for new peaks that are < X ATR from the roation extreme
						const channel_range = range(rotation_extreme_day, day);
						const channels = indicators.getChannels(
							x,
							channel_range,
							candidate_peaks.filter((peak_day) => peak_day > rotation_extreme_day),
							trend
						);

						if ((channels.days.length && channels.current_rotation_day) || channels.days.length > 1) {
							// at least two peaks since the rotation extreme
							// get the latest peak
							const subsequent_peak_day = Math.max(...channels.days, channels.current_rotation_day);

							if (subsequent_peak_day) {
								if (Math.abs(x[subsequent_peak_day][extreme_neg] - rotation_extreme) / ATR[subsequent_peak_day].y > large_rotation_filter_threshold_2) {
									// latest peak is at least X ATR from rotation extreme
									// reset trend to start from the large rotation extreme

									double_battle_line = false;
									trend_start_day = rotation_extreme_day;
									trend_range = range(trend_start_day, day);
									trend_extreme_day =
										trend_range[
											trend_range
												.map((day) => x[day][extreme_pos])
												.indexOf(trend * Math.max(...trend_range.map((day) => trend * x[day][extreme_pos])))
										];
									current_short_side = channels.current_rotation_size;

									res[day].Notes += (res[day].Notes ? "; " : "") + `Large rotation filter enabled`;
									res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support" : "resistance"} shifts`;

									rotation_extreme_day = channels.current_rotation_day ? channels.current_rotation_day : channels.days[channels.days.length - 1];

									if (channels.length > 1) {
										// multiple fully formed channels, select the larger of the last two

										if (channels.current_rotation_size >= channels.size[channels.size.length - 1]) {
											// current roation is larger than last channel, this sets trend defining
											trend_defining_day = channels.current_rotation_day;
										} else if (channels.size[channels.size.length - 1] >= channels.size[channels.size.length - 2]) {
											// most recent channel is larger than previous channel
											trend_defining_day = channels.day[channels.day.length - 1];
										} else if (channels.current_rotation_size) {
											trend_defining_day = channels.day[channels.day.length - 1];
											double_battle_line = true;
											battle_line_2_day = channels.current_rotation_day;
											battle_line_1_day = trend_defining_day;
										} else {
											trend_defining_day = channels.day[channels.day.length - 2];
											double_battle_line = true;
											battle_line_2_day = channels.day[channels.day.length - 1];
											battle_line_1_day = trend_defining_day;
										}

										// pvf set to last fully formed channel
										pvf = channels.size[channels.size.length - 1];
										pvf_peak_day = channels.day[channels.day.length - 1];
									} else if (trend * x[channels.current_rotation_day][extreme_neg] > trend * x[channels.days[0]][extreme_neg]) {
										// only one fully formed channel
										trend_defining_day = channels.current_rotation_size > channels.size[0] ? channels.current_rotation_day : channels.days[0];

										pvf = channels.size[0];
										pvf_peak_day = channels.days[0];
									} else {
										null;
									}

									rotation_extreme = x[rotation_extreme_day][extreme_neg];
									trend_defining_level = x[trend_defining_day][extreme_neg];
									pvf_peak = x[pvf_peak_day][extreme_neg];
									shift = false;
									one_time_shift_rule = false;
								}
							}
						}
					}
				}
			}

			if (extended_impulse_filter) {
				// TODO
				if (rotation_extreme_day > trend_defining_day) {
					if (trend * rotation_extreme_day > trend * trend_defining_level) {
						// we are shifting in the direction of the trend (lower for bear trend, higher for bull trend)
						trend_defining_day = rotation_extreme_day;
						trend_defining_level = rotation_extreme;
						one_time_shift_rule = false;

						res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts`;

						if (!res[day].Notes.includes("Extended impulse filter enabled")) {
							res[day].Notes += (res[day].Notes ? "; " : "") + `extended impulse filter`;
						}

						first_extended_shift = false;
					} else if (!one_time_shift_rule) {
						// can only shift opposite of the trend once so this needs to be false
						// marker to allow shifting or not
						let ots = false;

						// cehck if the current rotation is large enough
						if (trend_defining_day > trend_extreme_day || trend_extreme_day == day) {
							// this is either not a fully formed channel or we've made a new trend extreme
							if (current_short_side_day == rotation_extreme_day) {
								ots = true;
							}
						} else {
							// compare the current short side to the pvf, most recent channel
							if (current_short_side > pvf) {
								// roation is large enough to shift lower
								ots = true;
							}
						}

						if (ots) {
							trend_defining_day = rotation_extreme_day;
							trend_defining_level = rotation_extreme;

							// set one shift rule to true until we shift in the correct direction
							one_time_shift_rule = true;

							res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts, OTSR enabled`;

							if (!res[day].Notes.includes("Extended impulse filter enabled")) {
								res[day].Notes += (res[day].Notes ? "; " : "") + `extended impulse filter`;
							}

							first_extended_shift = false;
						}
					} else if (trend * x[day][extreme_pos] > Math.max(...range(trend_defining_day, day - 1).map((day) => trend * x[day][extreme_pos]))) {
						// we've made a new postive extreme for the rotation, resetting the pattern so we are able to shift
						trend_defining_day = rotation_extreme_day;
						trend_defining_level = rotation_extreme;

						res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts, OTSR enabled`;

						if (!res[day].Notes.includes("Extended impulse filter enabled")) {
							res[day].Notes += (res[day].Notes ? "; " : "") + `extended impulse filter`;
						}

						first_extended_shift = false;
					} else {
						res[day].Notes += (res[day].Notes ? "; " : "") + `Can't shift due to one time shift rule`;
					}
				}
			} else if (shift & (!bad_trend_filter | bad_trend_probe)) {
				// skip shifting if we are still in a bad trend
				if (trend * current_short_side_peak > trend * trend_defining_level) {
					// we are shifting in the direction of the trend (lower for bear trend, higher for bull trend)
					res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts`;

					// set trend defining parameters
					if (double_battle_line) {
						if (bad_trend_shift) {
							// double battle line has not been fully enabled yet
							trend_defining_day = battle_line_1_day;
						} else {
							trend_defining_day = new_extreme_battle_line ? battle_line_1_day : battle_line_2_day;
							new_extreme_battle_line = false;

							res[day].Notes += (res[day].Notes ? "; " : "") + `Double battle line`;
						}
					} else {
						trend_defining_day = rotation_extreme_day;
					}

					trend_defining_level = x[trend_defining_day][extreme_neg];

					// reset one time shift rule
					one_time_shift_rule = false;
				} else if (!one_time_shift_rule) {
					// can only shift in the opposite direction of the trend if the OTS filter is not enabled
					trend_defining_level = rotation_extreme;
					trend_defining_day = rotation_extreme_day;

					// one time shift rule is now enabled
					one_time_shift_rule = true;

					res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts, OTSR enabled`;
				} else if (trend * x[day][extreme_pos] > Math.max(...range(trend_defining_day, day - 1).map((day) => trend * x[day][extreme_pos]))) {
					// we've made a new postive extreme for the rotation, resetting the pattern so we are able to shift
					trend_defining_day = rotation_extreme_day;
					trend_defining_level = rotation_extreme;

					res[day].Notes += (res[day].Notes ? "; " : "") + `Trend ${trend == 1 ? "support " : "resistance "} shifts, OTSR enabled`;
				} else {
					res[day].Notes += (res[day].Notes ? "; " : "") + `Can't shift due to one time shift rule`;
				}

				// reset shifts variables
				shift = false;
				bad_trend_probe = false;
			}

			if (MA_override) {
				MA_override = (trend * (moving_avg[day].y - x[trend_defining_day][extreme_neg])) / ATR[day].y > extended_impulse_filter;
				if (MA_override) {
					res.map.Notes += (res[day].Notes ? "; " : "") + `MA override`;
				}
			}

			res[day] = {
				...res[day],
				Trend: trend,
				"Pattern Level": trend_defining_level,
				"Moving Average": moving_avg[day].y,
				ATR: ATR[day].y,
				"Trend Defining Level": pattern_override
					? trend_defining_level
					: MA_override
					? moving_avg[day].y
					: trend * Math.min(trend * trend_defining_level, moving_avg[day].y ? trend * moving_avg[day].y : Infinity),
				"Pattern Level Day": dateindex[trend_defining_day],
			};
		}

		// results cleanup

		//fs.writeFileSync("js_test.json", JSON.stringify(res, null, 2));

		results[asset] = res;
	});

	console.log("Done");
	return results;
};

// const res = getTrends(["TY1 Comdty", "USDJPY Curncy"]);

// console.log(res);
module.exports = { getTrends };
