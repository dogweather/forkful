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

## What & Why?

Sending an HTTP request with basic authentication is a way for programmers to securely access web services using a username and password. This method is commonly used to authenticate users and grant access to protected resources on a server.

## How to:

To send an HTTP request with basic authentication in C#, you can use the ```NetworkCredentials``` class along with the ```WebRequest``` class. Here's an example of how to make a GET request with basic authentication:

```
var request = WebRequest.Create("https://example.com/api/resource");
request.Credentials = new NetworkCredential("username", "password");
var response = request.GetResponse();
```

This code sets the credentials using the ```NetworkCredential``` class and makes a request to the specified URL. The response will contain the data from the requested resource.

## Deep Dive

Basic authentication has been around since the early days of the internet and is one of the simplest methods for authenticating users. It involves sending a base64-encoded string containing the username and password in the HTTP header. While this method is easy to implement, it has some security drawbacks, such as exposing the username and password in plain text.

An alternative to basic authentication is OAuth, which is more secure and supports token-based authentication. However, basic authentication is still commonly used for its simplicity and compatibility with a wide range of servers.

In C#, the ```WebRequest``` class handles all low-level HTTP operations such as sending and receiving requests. The ```NetworkCredential``` class provides a simple way to set the basic authentication credentials for a request.

## See Also

- [NetworkCredential Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.networkcredential)
- [WebRequest Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)