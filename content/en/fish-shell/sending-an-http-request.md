---
title:                "Sending an http request"
html_title:           "Fish Shell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a way for a computer program to communicate with another program or server over the internet. Programmers use this to retrieve data, make changes, or perform other actions on a remote server or web application.

## How to:
To send an HTTP request in Fish Shell, you can use the `curl` command. Here's an example of making a GET request to an API and saving the response to a variable named `response`:
```
set response (curl https://api.example.com)
```
You can then use this variable to access the data returned by the API. For example, to get the status code of the response:
```
echo $response_status
```

## Deep Dive:
HTTP requests have been around since the early days of the internet, and are a fundamental part of how web applications and services communicate with each other. In addition to `curl`, there are other tools and libraries that can be used to send HTTP requests in Fish Shell, such as `wget` and the `httpie` library. These tools offer different features and syntax, so it's worth exploring them to find the best fit for your use case.

Sending an HTTP request involves several steps, including establishing a connection, sending the request, and receiving and parsing the response. There are different types of HTTP requests, such as GET, POST, PUT, and DELETE, each serving a different purpose.

## See Also:
- [Curl Official Documentation](https://curl.se/docs/)
- [HTTP Requests with Wget](https://www.gnu.org/software/wget/manual/html_node/HTTP-Options.html)
- [HTTPie Library Documentation](https://httpie.io/docs)