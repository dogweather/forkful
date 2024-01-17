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

## What & Why?
Sending an HTTP request is simply a way for a programmer to communicate with other systems on the internet. It allows for retrieving and sending data between different servers and clients. Programmers often use this to access and manipulate data from external sources, such as APIs, databases, or web services.

## How to:
To send an HTTP request in Elm, we can use the HTTP module. First, we need to import it in our code:
```
import Http

```
Next, we can use the `send` function to create a request and send it to a specific URL:
```
Http.send
    { method = "GET"
    , headers = [ ]
    , url = "https://example.com/api"
    , body = Http.emptyBody
    , expect = Http.expectJson (\_ -> Json.Decode.succeed SuccessMsg)
    }
```

This code will send a GET request to `https://example.com/api`, with no headers and an empty body. The `expect` field specifies the type of data we expect to receive in response, in this case, a `SuccessMsg` defined beforehand.

The `send` function is asynchronous, meaning the code after it will continue executing while the request is being sent. To handle the response, we can use the `Task` module. Here's an example:
```
Http.send MsgDecoder <| Http.get "/api/user/123"
```

In this example, we use `Http.get` which is a shorthand for creating a GET request. `MsgDecoder` is a function that decodes the received data and triggers a `Msg` type with the decoded data. We can then handle this `Msg` type in our `update` function.

## Deep Dive:
The HTTP module was created to provide a simple and type-safe API for making HTTP requests. It also handles various errors and exceptions that might occur while communicating with external systems.

There are alternative ways to send HTTP requests in Elm, such as using libraries built on top of the HTTP module, or combining the HTTP module with the `Task` module. However, using the default HTTP module is recommended for simpler and more streamlined code.

The implementation details of the HTTP module involve using the Fetch API in the browser, which handles the actual sending of requests. In addition, the Elm compiler enforces a type system on HTTP requests, ensuring they are well-structured and will not fail due to type errors.

## See Also:
- Official Elm documentation for HTTP: https://package.elm-lang.org/packages/elm/http/latest/
- A beginner-friendly tutorial on using the HTTP module in Elm: https://elmprogramming.com/advanced/http.html
- A detailed explanation of how the HTTP module works under the hood: https://medium.com/@robertsosinski/the-internals-of-elm-http-90e2e5e97589