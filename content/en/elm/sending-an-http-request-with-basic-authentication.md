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

## What & Why?

Sending an HTTP request with basic authentication means including your username and password in the request headers for authentication purposes. This is commonly used by programmers when accessing APIs or web services that require user authentication.

## How to:

To send an HTTP request with basic authentication in Elm, you can use the `Http.send` function and pass in a `request` with the `BasicAuth` type.

Example code:

```elm
import Http
import Json.Decode exposing (int, string, decodeValue)
import Task exposing (Task)

authRequest : Http.Request
authRequest =
    Http.request
        { method = "POST"
        , body = Http.emptyBody
        , headers =
            [ ( "Authorization", "Basic username:password" )
            ]
        , url = "https://www.example.com/api"
        }

sendRequest : Task Http.Error String
sendRequest =
    Http.send (decodeValue string) authRequest

result : Task.Result Http.Error String
result =
    Task.attempt identity sendRequest

```

Sample output:

If the request was successful, the result will be the response from the API or web service. If there was an error, the result will be an `Http.Error` containing details about the error.

## Deep Dive:

1. Historical context:
Basic authentication has been around since the early days of the internet and was initially developed as part of the HTTP protocol. However, due to its security vulnerabilities, it is now considered a less secure form of authentication and is often replaced by more modern methods such as OAuth.

2. Alternatives:
As mentioned, basic authentication is not considered the most secure method of authentication. Some alternatives include OAuth, JSON Web Tokens (JWT), and API keys.

3. Implementation details:
In Elm, the `Http.send` function takes in a `request` as an argument, which can include the `Method`, `Url`, `Headers`, and `Body` of the request. The `Authorization` header is where the basic authentication details are specified in the format of "Basic username:password", where the username and password are encoded in Base64.

## See Also:

- Official Elm documentation on sending HTTP requests: https://package.elm-lang.org/packages/elm/http/latest/Http
- Understanding basic authentication: https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-nginx-server-blocks