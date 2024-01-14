---
title:                "Elm recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

If you are new to Elm programming, you may be wondering why someone would need to send an HTTP request. Well, HTTP requests are a crucial part of web development as they allow us to communicate with servers and retrieve data, which is necessary for building dynamic and interactive web applications.

## How To

Sending an HTTP request in Elm is a fairly straightforward process. Let's take a look at an example:

```elm
import Browser
import Http
import Json.Decode exposing (..)

type alias User = {
    id: Int,
    name: String,
    email: String
}

getUserInfo : Int -> Cmd Msg
getUserInfo userId =
    let
        url = "https://example.com/users/" ++ (toString userId)
        request = Http.get url userDecoder
    in
        Http.send UserFetched request

userDecoder : Decoder User
userDecoder =
    decode User
        |> field "id" int
        |> field "name" string
        |> field "email" string

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserFetched (Ok user) ->
            { model | user = Just user } ! []

        UserFetched (Err error) ->
            -- handle error

view : Model -> Browser.Document Msg
view model =
    div []
        [ button [ onClick (getUserInfo 1) ] [ text "Get User Info" ]
        , div [] [ text (toString model.user) ]
        ]
```

In this example, we are using the `Http` package to make a `GET` request to `https://example.com/users/{userId}`. The `getUserInfo` function takes in a user ID and returns a `Cmd Msg` which will eventually trigger the `UserFetched` message with the result of the HTTP request.

We also define a `userDecoder` which specifies the structure of the data we expect to receive from the server. This is necessary for decoding the JSON response into a `User` record.

In the `update` function, we handle the `UserFetched` message by updating our model with the retrieved user data. Finally, in the `view` function, we display a button that triggers the `getUserInfo` function and also display the retrieved user information on the page.

This is just a basic example of sending an HTTP request in Elm. There are many more options and features available, which we will cover in the next section.

## Deep Dive

The `Http` package in Elm allows us to make various types of HTTP requests such as `GET`, `POST`, `PUT`, `DELETE`, etc. We can also specify headers, attach data, and handle errors in a more detailed manner.

One important thing to keep in mind while sending HTTP requests in Elm is that they are asynchronous, meaning that they won't block the execution of other code. Instead, they will be handled by the Elm runtime and eventually trigger a message with the response.

To learn more about sending HTTP requests in Elm, I highly recommend checking out the official documentation and trying out different examples.

## See Also

- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest)
- [Sending HTTP requests with Elm](https://dev.to/victorlourng/sending-http-requests-with-elm-and-fetch-12ol) (external blog post)
- [Handling async requests in Elm](https://levelup.gitconnected.com/handling-async-requests-in-elm-ca74a7a0fd83) (external blog post)