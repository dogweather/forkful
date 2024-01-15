---
title:                "Sending an http request with basic authentication"
html_title:           "Elm recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests with basic authentication is a crucial aspect of web development. It allows secure communication between a client and a server, ensuring that only authorized users can access certain resources.

## How To

To send an HTTP request with basic authentication in Elm, we first need to understand the syntax for making HTTP requests in general. The following code block shows a basic example of sending an HTTP GET request and handling the response:

```Elm
import Http

type Msg = RequestFailed | RequestSucceeded String

makeRequest : Cmd Msg
makeRequest =
  Http.get
    { url = "https://www.example.com"
    , expect = Http.expectString RequestSucceeded
    }
```

Here, we import the `Http` module and define a `Msg` type to handle the response. Then, we use the `Http.get` function with the desired URL and `expect` function to specify the type of response we expect to receive. The `Http.expectString` function expects a `String` as its response, and in this case, we handle it by sending a `RequestSucceeded` message.

Now, to add basic authentication to our HTTP request, we need to add an `Authorization` header to our request. Here's an example of how we can do that:

```Elm
import Http
import String

type Msg = RequestFailed | RequestSucceeded String

makeRequest : Cmd Msg
makeRequest =
  Http.get
    { url = "https://www.example.com"
    , expect = Http.expectString RequestSucceeded
    , headers = [ Http.header "Authorization" (String.join " " ["Basic", "username:password"]) ]
    }
```

In this code block, we import the `String` module and add the `headers` field to our `Http.get` function call. Inside the `headers` field, we use the `Http.header` function to specify our `Authorization` header, which consists of the word "Basic" followed by a space, and then the encoded string of our username and password separated by a colon.

## Deep Dive

Now that we know how to add basic authentication to our HTTP requests, let's take a deeper look at what's actually happening behind the scenes. When we send a request with basic authentication, the server expects an `Authorization` header with a value of "Basic" followed by a space, and then the encoded string of our username and password. The encoded string is created using base64 encoding, which is a type of binary-to-text encoding that converts binary data into ASCII characters.

In Elm, we can use the `String.join` function to join our username and password with a colon, and then use the `String.toBase64` function to encode the resulting string.

## See Also

- [Elm HTTP module documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Base64 encoding in Elm](https://package.elm-lang.org/packages/elm-lang/bytes/latest/Bytes-Encode#toBase64)