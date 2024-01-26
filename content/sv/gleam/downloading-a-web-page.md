---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:44:05.765939-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess innehåll över internet till din egen dator. Programmakare gör detta för att bearbeta sidans information, automatisera uppgifter, eller samla data.

## Hur gör man:
```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() {
  // Defining the URL we're going to hit
  let url = "https://example.com"

  // Making a GET request
  case httpc.send(http.Request(method: Get, url: url)) {
    Ok(response) -> 
      io.println("Successfully downloaded the page!")
      response.body
        |> should.to_string
        |> result.unwrap
        |> io.println

    Error(error) ->
      io.println("Failed to download the page: ")
      error
        |> should.to_string
        |> result.unwrap
        |> io.println
  }
}
```
Output:
```
Successfully downloaded the page!
<!doctype html>...
```

## Fördjupning:
Downloading web pages isn't a new concept—it's been fundamental since the early days of the internet. Before Gleam and its brethren, tools like `wget` and `curl` dominated the scene, and languages like Python and PHP incorporated ways to perform this task through standard libraries.

In Gleam, downloading a web page is typically done using the `http` module, but there are alternatives like direct socket programming for more control. Under the hood, the operation involves making an HTTP GET request, which the server responds to with the contents. There's a lot going on with headers, response codes, and error handling that programmers must consider.

## Se också:
- Gleam's official HTTP documentation: https://hexdocs.pm/gleam_http/
- An intro to HTTP for programmers: https://developer.mozilla.org/en-US/docs/Web/HTTP
- `curl` for comparison with command line: https://curl.se/docs/
- `wget` manual for historical significance: https://www.gnu.org/software/wget/manual/wget.html
