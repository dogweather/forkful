---
title:                "Elm recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests with basic authentication allows for secure communication between a client and server. This is commonly used in web development to authenticate users and authorize access to certain resources.

## How To
To send an HTTP request with basic authentication in Elm, we first need to import the `Http` and `Request` modules. We can then create a `Request` record and specify the method, url, and headers for our request. We also need to provide a `Body` value, which can be a simple string or a JSON object.

```Elm
import Http
import Request

myRequest = { method = "GET"
            , url = "https://example.com/api"
            , headers = [ ("Authorization", "Basic dXNlcjpwYXNzd29yZA==") ]
            , body = Http.emptyBody
            }
```

In the above code, we have specified a `GET` request to the URL `https://example.com/api` with a basic authentication header containing a username and password encoded in base64. We can then use the `Http.send` function to actually send the request and handle the response.

```Elm
Http.send Http.expectString myRequest
```

This will return an `Http.Task` that we can use to handle the response. We can use the `Http.expectString` function to specify the expected response format, which in this case is a simple string. We can then handle the `Http.Task` using the `Task.attempt` function and pattern match on the result.

## Deep Dive
When using basic authentication, it is important to ensure that the username and password are encoded in base64 to prevent them from being easily decoded. It is also recommended to use HTTPS when sending the request to ensure that the credentials are not sent in plain text and vulnerable to interception.

Additionally, basic authentication is not as secure as other authentication methods such as OAuth2 or JWT. It is recommended to use these methods instead for more sensitive information.

## See Also
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [HTTP Basic authentication on Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)