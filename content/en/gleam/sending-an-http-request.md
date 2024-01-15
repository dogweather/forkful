---
title:                "Sending an http request"
html_title:           "Gleam recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

If you're working on a web application or backend system, chances are you will need to communicate with other services through HTTP requests. This can be anything from retrieving data from an API to sending data to another server. Thankfully, Gleam makes it easy to handle HTTP requests with its built-in functions. In this article, we'll go over the basics of sending an HTTP request in Gleam.

## How To

To send an HTTP request in Gleam, we first need to import the `gleam/http` module. Inside this module, we have the `send` function which allows us to make a basic HTTP request. Let's take a look at an example:

```Gleam
let result = http.send(
  method: "GET",
  url: "https://api.example.com/users",
  headers: [("Content-Type", "application/json")],
  body: None
)
```

In the above code, we are sending a `GET` request to the given URL which returns a list of users in JSON format. We also specify the `Content-Type` header to let the server know what type of data we are expecting. Finally, we set the request body to `None` as we are not sending any data. The `http.send` function returns a `Result` type, and we can pattern match to handle any potential errors.

Once we have successfully made the HTTP request, we can access the response data by using the `Body` module from the `gleam/http` package. Let's see an example:

```Gleam
case result {
  Ok(response) -> {
    let body = http.Body.String(response.body)
    // do something with the response body
  }
  Error(error) -> {
    // handle the error
  }
}
```

In this code, we are pattern matching on the `result` variable and accessing the `response` object. We can then use the `String` function from the `Body` module to convert the response body into a string which we can then manipulate as needed.

## Deep Dive

If you need to make more advanced HTTP requests such as adding custom headers, setting specific request timeout durations, or handling different HTTP methods, the `gleam/http` module has you covered. It provides functions such as `send_with` and `send_timeout` which allow you to specify these parameters. Additionally, you can also use the `decode` function to handle the response data and parse it into a desired data type.

## See Also
- [Gleam HTTP Module Documentation](https://gleam.run/modules/http.html)
- [Gleam Tutorial on HTTP Requests](https://gleam.run/articles/62)
- [HTTP Requests in JavaScript vs. in Gleam](https://medium.com/@jduhamel/http-requests-in-javascript-vs-in-gleam-2ad410f2ff6e)