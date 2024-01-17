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

## What & Why?

Downloading a web page means retrieving the content of a web page from a remote server and displaying it on our device. Programmers often do this in order to create dynamic and interactive web applications, as well as to retrieve data such as images, text, and videos from websites.

## How to:

The process of downloading a web page in Elm is quite simple. First, we need to import the `Http` module to handle HTTP requests. Then, we use the `Http.get` function to specify the URL of the web page we want to download. Finally, we use the `Html.program` function to render the downloaded content on our application's view.

```Elm
import Http
import Html exposing (..)

main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init =
    ( "https://www.example.com", Http.get "https://www.example.com" pageResponse )
 
update msg model =
    case msg of
        PageResponse response ->
            ( model, Cmd.none )

subscriptions model =
    Sub.batch
        [ Http.onLoad PageResponse
        ]

view model =
    case model of
        Loading ->
            text "Loading..."

        Success content ->
            text content

type Msg
    = PageResponse (Result Http.Error String)

-----------------------------------------------------------------------------------------------------------

type Model
    = Loading
    | Success String

-----------------------------------------------------------------------------------------------------------
 
pageResponse : Result Http.Error String -> Msg
pageResponse result =
    case result of
        Ok response ->
            PageResponse (Ok response.body)

        Err err ->
            PageResponse (Err err)
```

Sample Output:

The above code will retrieve the content of the web page from the specified URL and display it in the view of our web application. If the request is successful, the downloaded content will be rendered as a `Success` string, otherwise an error message will be displayed.

## Deep Dive:

Downloading web pages has been a fundamental aspect of web development since the early days of the Internet. Traditionally, developers used technologies like AJAX or jQuery to make requests to the server and retrieve data. However, with the rise of modern web frameworks like Elm, this process has become simpler and more efficient.

There are also alternative methods for downloading web pages in Elm, such as using third-party libraries like `elm-lang/html` or `elm/http`. The `Http.get` function used in the example above is a part of the `elm/http` library, which is specifically designed for handling HTTP requests and responses.

When making requests using the `Http.get` function, it is important to handle potential errors such as network connectivity issues or invalid URLs. By using `Result` types and pattern matching, we can easily handle errors and provide appropriate feedback to the user.

## See Also:

To learn more about downloading web pages in Elm, check out the official documentation for the `Http` module on the Elm website. You can also explore other useful libraries and tools for web development in Elm such as `elm/url` and `elm/parser`.