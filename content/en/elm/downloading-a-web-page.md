---
title:                "Downloading a web page"
html_title:           "Elm recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you're interested in building web applications using functional programming, then Elm is a great language to explore. With its type safety and declarative syntax, it's gaining popularity among developers. In this article, we'll focus on a common task in web development - downloading a web page - and how to do it in Elm.

## How To

First, we'll need to import the `Http` module, which contains functions for making HTTP requests. We can do this by adding the following line to the top of our file:

```Elm
import Http
```

Next, we'll create a `Request` using the `request` function. This function takes in two arguments - a `method` and a `url`.

```Elm
let request = Http.request
    { method = "GET"
    , url = "https://www.example.com"
    }
```

After creating our request, we can use the `send` function to actually make the request and receive a response.

```Elm
Http.send handleResponse request
```

In the `send` function, we pass in a `handleResponse` function as the first argument. This function will be called when the request is completed, and it will receive a `Result` type with either a `Http.Error` or a `Http.Response`. We can pattern match on this `Result` to handle the different scenarios.

```Elm
handleResponse : Result Http.Error Http.Response -> msg
handleResponse result =
    case result of
        Ok response ->
            case response.statusCode of
                200 ->
                    -- do something with the response body
                _ ->
                    -- handle other status codes
        Err error ->
            -- handle errors
```

## Deep Dive

Under the hood, Elm uses `XMLHttpRequest` to handle HTTP requests. This allows for a consistent API across browsers. Additionally, Elm automatically handles cross-origin requests, eliminating the need for CORS. It also handles caching and retries, making it easier to deal with common HTTP scenarios.

## See Also

- [Official Elm website](https://elm-lang.org/)
- [Elm documentation](https://package.elm-lang.org)
- [Elm subreddit](https://www.reddit.com/r/elm/)