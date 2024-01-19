---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a method used to protect online resources. It requires a username and password for access—the premise is simple: keep unauthorized users out and let authorized users in.

## How to:

Using Elm, basic auth-protected resources can be accessed via HTTP. Here's an example:

```Elm
module Main exposing (..)

import Http
import Http.Headers as Headers
import Json.Decode as Decode

type alias User =
    { username : String
    , password : String
    }

type Msg
    = GotUser (Result Http.Error User)

fetchUser : Cmd Msg
fetchUser =
    Http.request
        { method = "GET"
        , headers = [ Headers.authorization "Basic dXNlcjpwYXNzd29yZA==" ]
        , url = "https://api.example.com/user"
        , body = Http.emptyBody
        , expect = Http.expectJson GotUser userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "username" Decode.string)
        (Decode.field "password" Decode.string)
```

This will send an HTTP GET request to "https://api.example.com/user" with the appropriate basic authentication header.

## Deep Dive

The concept of Basic Authentication in HTTP has been around since the early days of the web. Originally described in 1995's RFC 1945 (HTTP/1.0), it's one of the simplest methods for HTTP access control.

Despite its age, Basic Authentication is practiced in modern development as an easy and straightforward solution. However, it’s not the most secure method because it involves sending a base64-encoded version of the username and password, which can be easily decoded. Options like OAuth 2.0 or JWT-based authentication can offer more security.

The code above shows Basic Authentication in Elm—an HTTP request using Elm’s built-in `Http` library. `Headers.authorization` is used to set an Authorization header with Basic Authentication details. Note the base64 string "dXNlcjpwYXNzd29yZA=="—this is "username:password" in base64. In a real-life scenario, the `username:password` combination should be base64 encoded dynamically.

## See Also

- [Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/Http) documentation for more powerful HTTP request crafting.
- [Http Authorization Header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization) on MDN for detailed info on authentication headers.
- [JWT Elm](https://package.elm-lang.org/packages/simonh1000/elm-jwt/latest/) and [OAuth 2.0](https://oauth.net/2/) for more secure authentication topics.