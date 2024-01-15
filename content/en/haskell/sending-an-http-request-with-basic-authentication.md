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

## Why

Sending an HTTP request with basic authentication allows you to securely access protected resources on a server. It is commonly used in applications that require user authentication, such as web services or APIs.

## How To

To send an HTTP request with basic authentication in Haskell, you will need to use the `http-client` and `http-client-tls` packages. First, import them in your code:

```Haskell
import Network.HTTP.Client 
import Network.HTTP.Client.TLS
```

Next, create a `Manager` object to handle the HTTP connections:

```Haskell
manager :: IO Manager 
manager = newManager tlsManagerSettings
```

Then, construct a `Request` object with the URL of the resource you want to access:

```Haskell
req :: Request 
req = parseRequest_ "https://example.com/protected/resource"
```

Now, add the basic authentication credentials to the request:

```Haskell
let username = "myusername"
let password = "mypassword"
let req' = applyBasicAuth username password req
```

Finally, use the `httpLbs` function to send the request and get the response:

```Haskell
response <- httpLbs req' manager
```

You can access the response body and status code using the `responseBody` and `responseStatus` functions respectively.

Sample output:

```
"Hello, you have successfully accessed the protected resource!"
200
```

## Deep Dive

Under the hood, the `applyBasicAuth` function adds an `Authorization` header to the request with the format `Basic <encoded_credentials>`. The credentials are encoded using the Base64 encoding scheme, which provides a simple form of encryption.

It is important to note that basic authentication is considered to be one of the least secure methods of authentication, as the credentials are sent in plain text. Therefore, it is recommended to use HTTPS along with basic authentication to ensure secure transmission of the credentials.

## See Also

- [HTTP-Client Package Documentation](https://hackage.haskell.org/package/http-client)
- [HTTP-Client-TLS Package Documentation](https://hackage.haskell.org/package/http-client-tls)
- [HTTP Authentication Protocols](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)