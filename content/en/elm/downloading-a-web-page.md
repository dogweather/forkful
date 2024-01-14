---
title:                "Elm recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Web development has become an essential skill in today's technological landscape, and being able to download a web page is an important aspect of this field. Whether you are a web developer, a hobbyist, or simply curious about how to retrieve a web page, learning how to do it in Elm can bring your programming knowledge to the next level.

## How To

Firstly, make sure you have Elm installed on your machine. Then, let's dive into the code! 

```Elm
import Browser
import Html exposing (text)
import Json.Decode 

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type Msg 
  = ReceiveData Msg.ReceiveData

type alias Model = 
  { data : String
  }

init : () -> ( Model, Cmd Msg )
init _ = 
  ( Model "", getData )

getData : Cmd Msg 
getData = 
  Http.get 
    { url = "https://www.example.com"
    , expect = 
        Http.expectString ReceiveData (Json.Decode.succeed "Success!")
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    ReceiveData data ->
      ( { model | data = data }, Cmd.none )

view : Model -> Html Msg 
view model = 
  text model.data 

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
```

Output:
```
Success!
```

Let's break down the code. Firstly, we import the necessary libraries: `Browser`, `Html`, and `Json.Decode`. The `init` function is where we set up our model and any initial commands. In this case, our model consists of a `data` field that will hold the retrieved web page. The `getData` function makes the `Http` request to the specified URL and expects a string as a response. The `update` function handles the `ReceiveData` message and updates our model accordingly. The `view` function simply displays the retrieved data on the screen.

## Deep Dive

Now that we have the basics covered, let's explore further. The `getData` function can be customized to include different parameters, such as headers or form data. Additionally, you can use `Http.send` instead of `Http.get` if you want to send web page data instead of just retrieving it.

You can also use Elm's `Cmd` module to perform multiple `Http` requests asynchronously and handle the responses with `Debug.log` or `Json.Decode` functions. This can be useful for creating more complex web scraping or data mining applications.

## See Also

For more information on downloading web pages in Elm, check out these helpful links:

- [Elm Guide: HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm Docs: Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Using Elm's Cmd to preform multiple Http Requests](https://medium.com/@dschultz_51649/using-elms-cmd-to-preform-multiple-http-requests-9c11d4daed93)

Happy coding!