function getData(assets) {
	// TODO create bbg request for data here
	assets = Array.isArray(assets) ? assets : [assets];
	const test_data = require("../../data/test_data.json");
	const res = {};

	assets.forEach((asset) => {
		res[asset] = test_data;
	});

	return res;
}

module.exports = { getData };
