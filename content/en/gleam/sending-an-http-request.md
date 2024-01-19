---
title:                "Sending an http request"
html_title:           "Gleam recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Light Up Your Coding With Gleam: Sending HTTP Requests 

## What & Why?

In the programming world, sending an HTTP Request is the act of requesting a particular action from a server, such as fetching, updating or deleting data. Whether you're working on a weather app or a food delivery service, you'll use HTTP requests to interact with live data from various APIs.

## How to:

Here is a simple example of making a `GET` request with Gleam's `gleam/httpc`:

```gleam
import gleam/httpc.{get}

pub fn fetch_data() {
  let response = get("https://api.example.com/data")
  case response {
    Ok(response) -> io.println(response.body)
    Error(err) -> io.println(err)
  }
}
```

To run this code, call `fetch_data()` in your main function. The output will show the body of the http response, or the error if there is one.

## Deep Dive

Historically, the HTTP's simple design was a major factor in the rapid adoption and widespread use of the internet. As for the alternatives, there are other data transfer protocols like FTP, but HTTP(S) tends to be the go-to due to its simplicity, statelessness, and interoperability.

In terms of sending HTTP requests in Gleam, the `get` function used in our example is asynchronous, making a non-blocking request. It returns `Result(HttpClient.Response, HttpClient.Error)`, representing either a successful response or an error. 

## See Also

For further reading, check out these great resources on the subject:

- Gleam's official docs on HTTP Requests: [Gleam Http Library](https://hexdocs.pm/gleam_http/)
- The history and evolution of HTTP: [HTTP - Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- A comparison of data transfer protocols: [Comparison of Data Transfer Protocols](https://en.wikipedia.org/wiki/Comparison_of_data_transfer_protocols)