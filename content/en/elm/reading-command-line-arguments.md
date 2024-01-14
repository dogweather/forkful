---
title:    "Elm recipe: Reading command line arguments"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Command line arguments are an essential part of many programming languages, including Elm. Understanding how to read these arguments can greatly enhance your programming skills and allow you to create more dynamic and interactive applications.

## How To
Reading command line arguments in Elm is a straightforward process. Let's take a look at an example:

```elm
import Platform exposing (Arg)
import String

main : Program Never Model Msg
main =
    Platform.worker
        { init = \_ -> ("", Cmd.none)
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = \model -> ""
        , onUrlRequest = \_ -> ( init, Cmd.none )
        , viewMetadata = \_ -> { title = "MyApp", bodyClasses = []
                               , attributes = []
                               }
        , delta = MyCalculator
        }

type alias Model =
    String

type Msg
    = Set String
    
decoder : Decoder Model
decoder = String
    
calc : Model -> Html Msg
calc user =
  div [][
    div [ id "Welcome-Banner" ][
      text ("Welcome to your app, " ++ user)
    ]
]

main : Program Never Model Msg
main =
  Platform.worker
    {
      init =
        \_ ->
          (String.fromList [], Cmd.none)

      update =
        greatAccept

      view =
        \string ->
              calc string
    }


  
```

In this example, we are using the Platform module to access the command line arguments. The first line imports the necessary Arg module, and the String module will be used to manipulate the arguments. The main function sets up a worker with some basic functions. We have defined our Model as a string, and our Msg as a Set string. The decoder is simply a string decoder, as we are only dealing with one argument in this example.

The Calc function uses the user input to create a custom welcome message. Finally, in the main function, we have the update function, which will update the model with the user's input. We then have the view function, which uses the calc function to display the welcome message.

To run this code, we would need to compile it and then run it in the command line with the arguments of our choice. For example, we could run the following commands:

```bash
elm make Main.elm --output=main.js
node main.js "John"
```

The output would be a simple HTML page with the welcome message "Welcome to your app, John" displayed.

## Deep Dive
Under the hood, the command line arguments are stored in the Arg package as a list of strings. This list can then be easily manipulated using the String module. You can also use the Platform's args function to access the argument list directly. Additionally, by using the Cmd module, you can also pass arguments to other functions within your program.

It is essential to note that any arguments passed in the command line must be properly escaped with quotation marks, especially if they contain spaces or special characters.

## See Also
- Official Elm documentation on reading command line arguments: https://elm-lang.org/docs/from-javascript
- A beginner's guide to Elm: https://guide.elm-lang.org/
- More advanced examples of using Elm in real-world applications: https://github.com/mdgriffith/elm-style-animation