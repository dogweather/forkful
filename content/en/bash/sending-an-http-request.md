---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a key method of transferring data over the web - it's like sending a letter via the "postal service" of the internet. Programmers do it to interact with web services, APIs, or webpages, be it for retrieving info, sending data, or other operations.

## How to:
In Bash, you can use `curl` to send HTTP requests. To send a GET request:
```Bash
curl -X GET http://jsonplaceholder.typicode.com/posts/1
```
This fetches the post with ID 1. You will see output like:
```Bash
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit ..."
}
```
To send a POST request:
```Bash
curl -X POST -H "Content-Type: application/json" -d '{"title":"foo","body":"bar","userId":1}' http://jsonplaceholder.typicode.com/posts
```
This creates a new post. Output might be:
```Bash
{
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

## Deep Dive
HTTP requests date back to HTTP v1 (1991) when surfing webpages was the main intent. Now, they support web services, RESTful APIs, and more. Besides `curl`, you have alternatives like `wget` or using programming languages with HTTP support (Python requests, Node.js http).

`curl` is open-source, supports many protocols (not just HTTP) and provides advanced features like proxy support. When you run a `curl` command, your Bash shell fires up a curl process which handles the HTTP request, and outputs the response back to your shell.

## See Also
- `curl` command docs: https://curl.se/docs/manpage.html
- HTTP spec (very technical): https://tools.ietf.org/html/rfc2616
- "HTTP Made Really Easy": http://www.jmarshall.com/easy/http/ - simpler explanation of HTTP.