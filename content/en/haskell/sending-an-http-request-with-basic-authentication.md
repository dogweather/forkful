---
title:                "Haskell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests with basic authentication is an essential skill for any Haskell programmer who wants to interact with web APIs and services that require user authentication. It allows you to securely access protected resources and perform operations on behalf of a user.

## How To
To send an HTTP request with basic authentication in Haskell, we will be using the popular HTTP client library called `http-conduit`. First, we need to import the necessary modules:

```Haskell
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Client.TLS
```

Next, we need to create a `Request` object with the URL of the resource we want to access:

```Haskell
req <- parseRequest "https://api.example.com/user"
```

We also need to specify the HTTP method, headers, and body of the request. In this case, we will be using the `GET` method and setting the `Authorization` header with the username and password in the format `username:password`. We will use the `setHeaders` function to add the `Authorization` header and the `setQueryString` function to add any query parameters:

```Haskell
let req' = setRequestMethod "GET" req
         & setHeaders [("Authorization", "john:secret")]
         & setQueryString [("sort", Just "date"), ("limit", Just "10")]
```

Now, we need to make the request using the `httpLbs` function which takes the `Request` object as an argument and returns a `Response` object:

```Haskell
res <- httpLbs req' manager
```

Note that we are using a `manager` object which manages the connection pool for multiple HTTP requests.

Finally, we can get the response body using the `responseBody` function and use it as needed:

```Haskell
let resBody = responseBody res
-- do something with the response body
```

Here's a complete example of sending an HTTP request with basic authentication using `http-conduit`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    req <- parseRequest "https://api.example.com/user"
    let req' = setRequestMethod "GET" req
             & setHeaders [("Authorization", "john:secret")]
             & setQueryString [("sort", Just "date"), ("limit", Just "10")]
    res <- httpLbs req' manager
    let resBody = responseBody res
    putStrLn $ show resBody
```

The output of this code would be the JSON response from the API endpoint.

## Deep Dive
Now that we've seen how to send an HTTP request with basic authentication in Haskell, let's take a deeper look at the `setHeaders` function. It takes a list of tuples in the format `(headerName, headerValue)` and adds these headers to the request. In our example, we set the `Authorization` header with the username and password in the format `username:password`, as required by basic authentication.

While sending sensitive information like passwords over insecure HTTP connections is not recommended, basic authentication can be used with HTTPS to ensure secure transmission of credentials.

## See Also
- [Guide to HTTP authentication in Haskell](https://haskell-lang.org/library/http-client#a-simple-http-client)
- [Documentation for http-conduit library](http://hackage.haskell.org/package/http-conduit)
- [HTTP client tutorial for Haskell](https://github.com/snoyberg/http-client-tutorial)