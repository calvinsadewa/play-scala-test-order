# play-scala-test-order

Assumption:
1. customer,product,and coupon is uniquely identified by id (int)
2. order transaction backend is accessed by other backend, and use API_KEY query string for authentication

Test url:
1. https://evening-atoll-44036.herokuapp.com
2. https://evening-atoll-44036.herokuapp.com/browse/get/12?API_KEY=Magi3iewdc8icasd
    
See app.controller.OrderTransactionController and app.conf.routes
 
TODO : test, API for set get product and coupon , update database to heroku postgres
