var express = require("express");
var router = express.Router();

/* GET home page. */
router.get("/", function (req, res, next) {
	// res.render('index', { title: 'Express' });
	res.send('Hello World Go to - <a href="http://localhost:8081/ex-async"> http://localhost:8081/ex-async</a>');
});

module.exports = router;
