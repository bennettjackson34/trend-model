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

module.exports = { range, lead, lag };
