---
title:                "Sending an http request with basic authentication"
html_title:           "Haskell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a way for a programmer to access secure web resources by providing a username and password. This is necessary for websites that require user authentication, such as online banking or social media accounts. By including basic authentication in their code, programmers can automate the process of logging in and accessing these resources, saving time and effort in the long run.

## How to:

To send an HTTP request with basic authentication in Haskell, we can use the [http-client](https://hackage.haskell.org/package/http-client) library. First, we need to import the necessary modules:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
```

Next, we can create a new manager using the `newManager` function from `Network.HTTP.Client.TLS`. This will handle our HTTPS connections. Then, we can use the `parseUrlThrow` function to parse the URL of the resource we want to access. We can set the request method to `GET` and add basic authentication headers using the `applyBasicAuth` function. Finally, we can use `httpLbs` to send the request and receive a response:

```Haskell
main = do
  manager <- newManager tlsManagerSettings
  request <- parseUrlThrow "https://example.com/resource"
  let requestWithAuth = applyBasicAuth "username" "password" request
  response <- httpLbs requestWithAuth manager
```

The `response` variable will contain the response body as well as the status code, which we can access using `responseBody` and `statusCode` respectively.

## Deep Dive:

In the early days of the internet, basic authentication was widely used to secure web resources. However, it has since been replaced by more secure methods such as OAuth. Nevertheless, basic authentication is still commonly used and is supported by most web browsers and web servers.

There are alternative methods for sending HTTP requests with authentication, such as Digest authentication and OAuth. These methods provide stronger security by encrypting the credentials and using more advanced verification techniques.

The basic authentication header, which is added to the request in the example above, contains the username and password encoded in Base64 format. This means that the credentials are not encrypted and can be easily decoded, making it less secure compared to other methods.

## See Also:

- [http-client documentation](https://hackage.haskell.org/package/http-client)
- [Understanding Basic Authentication](https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-the-digest-module-for-apache)
- [OAuth 2.0 vs Basic Authentication](https://www.oauth.com/oauth2-servers/making-authenticated-requests/basic-vs-bearer/)