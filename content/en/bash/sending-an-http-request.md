---
title:                "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
In today's digital world, sending HTTP requests is a crucial part of any web application or website. It allows users to communicate with servers and retrieve the desired information or perform specific actions. As a Bash programmer, learning how to send HTTP requests can greatly enhance your scripting capabilities.

## How To
Sending an HTTP request with Bash is a straightforward process using the built-in curl command. It is a powerful tool that allows you to make various types of requests, such as GET, POST, PUT, DELETE, and more. Let's take a look at some coding examples and the corresponding output to understand better.

```Bash
# Sending a GET request
curl www.example.com

# Output
<!doctype html>
<html>
<head>
  <title>Example Website</title>
</head>
<body>
  <h1>Welcome to Example Website</h1>
</body>
</html>
```

In the above example, we are making a GET request to the website "www.example.com," and the output is the HTML code of the webpage.

```Bash
# Sending a POST request with data
curl -X POST -d "username=johndoe&password=secret" www.example.com/login

# Output
{"login_status": true}
```

In this example, we are making a POST request to the "/login" endpoint of "www.example.com" with the data "username=johndoe&password=secret." The response is a JSON object with the key "login_status" and value "true."

## Deep Dive
Now, let's take a deep dive and explore some of the essential concepts related to sending HTTP requests with Bash.

### HTTP Methods
As mentioned earlier, the curl command allows you to make various types of requests. These requests are based on the HTTP methods, such as GET, POST, PUT, and DELETE. GET requests are used to retrieve data, POST requests are used to send data, PUT requests are used to update existing data, and DELETE requests are used to delete data.

### Request URLs
The URL is an essential component of any HTTP request. It specifies the location of the resource on the server that the client wants to access. URLs consist of a protocol, domain name, and a specific path to the resource. For example, in the above examples, "http://" is the protocol, "www.example.com" is the domain name, and "/" or "/login" is the path.

### Request Headers
HTTP headers are used to provide additional information about the request or the requested resource. They can contain data such as the content type, user agent, and authorization credentials. You can add custom headers to your HTTP requests using the "-H" flag with the curl command.

## See Also
- [cURL Documentation](https://curl.haxx.se/docs/manpage.html)
- [HTTP Requests with Bash Scripting](https://www.baeldung.com/http-request-bash)
- [Anatomy of an HTTP Request](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview#HTTP_requests)