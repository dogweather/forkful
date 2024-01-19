---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?  
An HTTP request is a way for your program to communicate with a web server by sending a request and receiving a response. Programmers send HTTP requests to interact with APIs, fetch data, or post data onto a server.  

## How To:
In Gleam, we can use the `gleam/httpc` package. Here's a basic example:

```Gleam
import gleam/httpc

let get_request() {
    httpc.get("http://example.com")
    |> result.unwrap_to_string
}
```
This will send a GET request to `http://example.com`.

To send a POST request with JSON data, we can do something like this:

```Gleam
import gleam/httpc.{Headers, Post}
import gleam/map
import gleam/uri.{Uri}

let post_request() {
  let headers = Headers.new(map.from_list([tuple("Content-Type", "application/json")]))
  let uri = Uri.parse("https://jsonplaceholder.typicode.com/posts")
  let body = "{\"title\": \"foo\", \"body\": \"bar\", \"userId\": 1}"
  let request = Post.new_request(uri, body) |> Post.set_headers(headers)

  httpc.send(request)
  |> result.unwrap_to_string
}
```
This sends a POST request to `https://jsonplaceholder.typicode.com/posts` with JSON data.

## Deep Dive
HTTP requests aren't new. They've been part of web communication since HTTP v1.0 back in the '90s. Today, programmers have various ways to send HTTP requests—in Gleam, `httpc` is our go-to package, but alternatives like `curl` or `axios` exist in other languages.

The `httpc` package allows flexible interaction with servers using Gleam’s strong typing system. You set up the request, send it, and get a response back that you can handle as you wish.

## See Also
For more info on the `httpc` package, check out the [Gleam HTTP package](https://hexdocs.pm/httpc/readme.html) docs. For more on Gleam itself, here's its [GitHub Repository](https://github.com/gleam-lang/gleam). Interested in HTTP specifications? The [HTTP/1.1 RFC]( https://tools.ietf.org/html/rfc2616) is worth your time.