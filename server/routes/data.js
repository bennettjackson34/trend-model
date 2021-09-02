var express = require("express");
var router = express.Router();

const { getData } = require("../scripts/getTrends");

/* GET users listing. */
router.get("/", function (req, res, next) {
	res.send("respond with a resource");
});

router.get("/historical", function (req, res, next) {
	let asset = req.query.asset;
	const result = getData(asset);
	console.log("sending result");
	res.send(result);
});

module.exports = router;
