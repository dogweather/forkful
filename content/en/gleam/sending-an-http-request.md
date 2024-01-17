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

## What & Why?

Sending an HTTP request is a fundamental task for web developers. It involves requesting resources, such as files or data, from a server in order to build a website or web application. This is done through the use of the HTTP protocol, which allows for communication between a client (e.g. a web browser) and a server.

Programmers send HTTP requests to obtain information or perform actions, such as retrieving data from a database or posting form data. This process is essential for creating dynamic and interactive websites, as well as for integrating various systems and services.

## How to:

To send an HTTP request in Gleam, we will need to use the built-in `Http` module. This module provides functions for making different types of HTTP requests, such as `get`, `post`, `put`, and `delete`.

### Example 1: Making a GET request

```Gleam
result: Http.Response.t(H) =
  Http.get("https://jsonplaceholder.typicode.com/posts/1")
```

### Sample Output:

```Gleam
result := Ok
```

In this example, we make a `get` request to the JSONPlaceholder API and store the result in a variable called `result`. The `get` function takes in the URL of the resource we want to request and returns an HTTP `Response` containing the data from that resource.

### Example 2: Making a POST request

```Gleam
result: Http.Response.t(H) =
  Http.post("https://jsonplaceholder.typicode.com/posts",
    Http.postBodyJson({id=1, title="Sample Post", body="This is a sample post."}))
```

### Sample Output:

```Gleam
result := Created
```

In this example, we make a `post` request to the JSONPlaceholder API with the JSON data containing the properties of a new post. This time, the `post` function also returns an HTTP `Response`, but the response code is `Created` indicating that the post was successfully added.

## Deep Dive:

Sending HTTP requests has been a crucial aspect of web development since the early days of the internet. In the past, developers used to make requests using lower-level network programming languages such as C or Perl. However, with the rise of web development frameworks and high-level languages, the process has become much simpler and more accessible.

While Gleam provides a convenient and efficient way to make HTTP requests, there are other popular libraries and tools available for this purpose. Some examples include `ureq`, `rustful`, and `hyper`.

Internally, Gleam uses the powerful Rust library `hreq` for handling HTTP requests and responses. This allows for a robust and performant implementation of HTTP communication in Gleam.

## See Also:

- Gleam HTTP Module documentation: https://gleam.run/documentation/library/http/
- `hreq` library documentation: https://docs.rs/hreq/
- Using Gleam to build a REST API: https://medium.com/@naodev12/creating-a-rest-api-with-gleam-16beb17b5be3