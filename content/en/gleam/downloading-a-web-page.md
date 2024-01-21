---
title:                "Downloading a web page"
date:                  2024-01-20T17:44:01.019284-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching its content via HTTP. Programmers do this for web scraping, data analysis, or to interact with web services.

## How to:

Let's grab a web page using Gleam with the `gleam_http` package. Assume `gleam_http` and `gleam_otp` are already in your project dependencies.

```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() -> Result(String, Nil) {
  let response = httpc.send(http.Request(to: "http://example.com")) // Make the GET request
  should.equal(response.status, 200) // Confirm the status code is OK
  Ok(response.body) // Return the body of the response
}

```

Sample output after running your code might look like this:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive

Way back when, in the early days of the web, downloading a page was as simple as telnetting to port 80. Today, you have libraries and languages, like Gleam, that take care of the nitty-gritty HTTP details.

Alternatives to `gleam_http` include lower-level libraries or interfacing with other Erlang/Elixir libraries using Gleam's interoperability features.

The `gleam_http` function `httpc.send()` is doing the heavy lifting in our example. It's crafted atop the Erlang `httpc` module, providing a straightforward API with a smattering of Gleam's type safety and pattern matching.

## See Also

- Gleam documentation: https://hexdocs.pm/gleam/gleam_http/
- `gleam_http` GitHub repo: https://github.com/gleam-lang/http
- A primer on HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- For an in-depth look at web scraping, check out Beautiful Soup for Python: https://www.crummy.com/software/BeautifulSoup/