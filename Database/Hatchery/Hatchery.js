d3.csv('https://raw.githubusercontent.com/dana6691/shinyR/master/micro3.csv?token=AHAYWYELYGRMXYGIMKMNPP26R5WHE',
  d3.autoType)
  .then(function (data) {
    console.log(data)
  });