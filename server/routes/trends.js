var express = require("express");
var router = express.Router();

const { getData } = require("../scripts/getData");
const { getTrends } = require("../scripts/getTrends");

/* GET trends */

router.get("/historical", function (req, res, next) {
	let data = getData(req.query.asset);

	const result = getTrends(data);
	console.log("sending result");
	res.send(result);
});

module.exports = router;
