---
title:                "Fish Shell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why 
Sending HTTP requests is a crucial task for any web developer or programmer. It allows us to communicate with servers and retrieve data or perform actions on websites.

## How To 
To send an HTTP request using Fish Shell, we can use the `curl` command. It stands for "client URL" and is a popular tool for sending and receiving data through URLs.

```
curl -X GET https://www.example.com/
```

This command sends a GET request to the website `www.example.com` and retrieves the data from its homepage. We can also specify different methods, such as POST or PUT, and add parameters to our request.

```
curl -X POST -d "name=John&age=30" https://www.example.com/api/users
```

Here, we are sending a POST request to the `/api/users` endpoint of the website, with parameters `name` and `age`. This can be useful for creating new records or submitting user input to a server.

We can also add headers to our request using the `-H` flag. This is often necessary for authentication purposes or to pass additional information to the server.

```
curl -H "Authorization: Bearer TOKEN" https://www.example.com/api/profile
```

This command adds the `Authorization` header with a token to our request, allowing us to access a protected endpoint on the website. 

## Deep Dive 
Behind the scenes, sending an HTTP request involves creating a TCP connection to the server and sending a request in a specific format. The server then responds with a status code and possibly some data.

Fish Shell's `curl` command handles this process for us and abstracts away the technical details. However, it's important for us to understand the different parts of an HTTP request (method, URL, headers, and body) to use it effectively.

We can also use `curl` to test and debug our own APIs or troubleshoot any issues with a website. It provides us with detailed information about our request and the server's response, including the status code, headers, and duration of the request.

## See Also 
- [cURL - Official Website](https://curl.se/)
- [HTTP Requests - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [cURL Tutorial - Codecademy](https://www.codecademy.com/learn/learn-the-command-line/modules/learn-the-command-line-curl)