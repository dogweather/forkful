---
title:                "Sending an http request with basic authentication"
html_title:           "C# recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

If you're building a web application that needs to access resources protected by basic authentication, you'll need to send an HTTP request with the appropriate credentials. Basic authentication is one of the simplest ways to secure access to web resources and is widely supported by web servers.

## How To

To send an HTTP request with basic authentication in C#, you can use the `HttpClient` class from the `System.Net.Http` namespace. First, create an instance of `HttpClient`:

```C#
var client = new HttpClient();
```

Then, set the `Authorization` header with the user's credentials:

```C#
var username = "myusername";
var password = "mypassword";
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes($"{username}:{password}")));
```

Finally, use the `GetAsync()` method to send the request and get the response:

```C#
var response = await client.GetAsync("https://example.com/resource");
```

The response will contain the requested resource or an error message if the authentication failed. Here's an example of a successful response:

```
HTTP/1.1 200 OK
Date: Wed, 19 Feb 2020 15:24:17 GMT
Server: Apache/2.4.38 (Unix)
Content-Length: 196
Content-Type: text/html; charset=utf-8

<html>
<body>
<h1>Hello World!</h1>
</body>
</html>
```

## Deep Dive

When you set the `Authorization` header with the user's credentials, you're using the basic authentication scheme defined in the HTTP specifications. These credentials are encoded in base64 and sent in plain text, meaning they're not secure. For this reason, it's recommended to use HTTPS to encrypt the communication and protect the credentials from being intercepted.

It's also worth noting that the `Authorization` header can be set manually for each request, or you can create a `HttpClient` with credentials already set, using the `HttpClientHandler` class. This is useful if you need to make multiple requests with the same credentials.

## See Also

- [HttpClient Class (System.Net.Http)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Basic Authentication Scheme](https://tools.ietf.org/html/rfc7617)