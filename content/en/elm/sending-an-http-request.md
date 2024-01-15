---
title:                "Sending an http request"
html_title:           "Elm recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a crucial part of modern web development. Whether you are retrieving data from an external API or posting information to a server, understanding how to send HTTP requests in Elm is essential for building dynamic and interactive web applications.

## How To

To send an HTTP request in Elm, we first need to import the `Http` module.

```Elm
import Http
```

Next, we can use the `send` function from the `Http` module to create and send our request. It takes two arguments, the first being the configuration for the request and the second being a decoder that will handle the response.

```Elm
request : Http.Request String
request =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/todos/1"
        , expect = Http.expectString id
        }

send request
```

In this example, we are making a `GET` request to the URL of the JSON placeholder API, which will return the first todo item in the form of a string. We use `Http.expectString id` as our decoder, which simply returns the raw string response without any changes.

Once the request is sent, it will return a response in the form of an `Http.Response` type, which we can handle using the `send` function. Here is an example of how we can handle the response and display it on the page using `Html`.

```Elm
Html.text response.body
```

This will display the response body on the page, which in this case would be the string "the first todo item".

## Deep Dive

The `Http` module in Elm comes with various functions that allow us to customize and handle different types of requests, such as `get`, `post`, `put`, and `delete`. We can also add headers and data to our request using the `Http.Header` and `Http.stringBody` functions.

Additionally, Elm provides a built-in error handling mechanism for HTTP requests, using the `Http.Error` type. This allows us to handle various error scenarios, such as network failures, timeouts, and invalid or unexpected responses.

## See Also

- `Http` module documentation: https://package.elm-lang.org/packages/elm/http/latest/
- Guide to building web apps with Elm: https://guide.elm-lang.org/webapps/