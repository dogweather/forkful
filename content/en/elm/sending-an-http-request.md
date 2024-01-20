---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
HTTP requests allow programs to talk to the outside world, grabbing content like web pages or API data. Programmers use them to fetch data from external sources or interact with web services.

## How to:
In Elm, we mainly use the `Http` package for creating and sending an HTTP request. Instal the Http package:

```Elm
elm install elm/http
```

Import the Http module in your code:

```Elm
import Http
```
  
A simple HTTP GET request in Elm:

```Elm
get : String -> Task Http.Error String
get url =
    Http.get
        {
            url = url,
            expect = Http.expectString
        }
```

The `get` function requests a URL and expects a response as a string.

For execution of the HTTP task and handling of the HTTP result:

```Elm
main =
    get "https://api.github.com/users/elm"
        |> Task.attempt HandleResponse
```
Here, "https://api.github.com/users/elm" is the URL we're requesting. The `get` function returns a task, which we attempt with a `HandleResponse` function that we'd define elsewhere. 

## Deep Dive
HTTP requests have been integral to web development since Tim Berners-Lee cemented HTTP as the core protocol of the web. In the Elm language, sending HTTP requests is usually done using the `Http` library, but other libraries, like `elm-http-builder` or `elm-ajax`, can be used for more complex cases. 

Under the hood, Elm's `Http` library uses JavaScript's Fetch API (or XMLHttpRequest for older browsers) for sending HTTP requests. The result is then channeled back into the Elm world as a `Task`, which is a model for asynchronous operations that can succeed or fail. 

However, unlike in JavaScript where an HTTP request runs automatically once created, Elm chooses to make HTTP requests “cold”. This means HTTP requests don't do anything until they're given explicit permission. This reflects Elm's overall philosophy of having no side effects by default, ensuring a consistent and predictable behavior.

## See Also
- [Elm Guide: The Http package](https://guide.elm-lang.org/effects/http.html) 
- [MDN: Using Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [GitHub: elm-http-builder](https://github.com/lukewestby/elm-http-builder)