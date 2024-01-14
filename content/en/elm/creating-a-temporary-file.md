---
title:    "Elm recipe: Creating a temporary file"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to create a temporary file in your Elm program? Maybe you're building an app that needs to generate a file for a user to download or you're working with an API that requires a temporary file for uploading data. Whatever the reason may be, knowing how to create a temporary file in Elm can be a useful skill to have in your programming arsenal.

## How To

Creating a temporary file in Elm is relatively straightforward using the `File` module provided by the `elm/file` package. First, let's import the necessary modules and define our initial model:

```Elm
import File
import File.Selector as Selector
import File.System as System

type alias Model =
    { tempFile : Maybe File.Ref
    , errorMsg : Maybe String
    }
```

Next, we'll need to add a button to our view that will trigger the creation of our temporary file:

```Elm
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CreateTempFile ] [ text "Create Temporary File" ] ]
```

Now, we can define our `Msg` type and update function to handle the creation of the temporary file:

```Elm
type Msg
    = CreateTempFile
    | TempFileCreated (Result String File.Ref)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTempFile ->
            ( model, Cmd.fileSelect TempFileCreated "text/plain" )

        TempFileCreated (Ok fileRef) ->
            let
                msg =
                    case System.write "This is a temporary file." fileRef of
                        System.WriteSuccess ->
                            TempFileCreated (Ok fileRef)

                        System.WriteFail err ->
                            TempFileCreated (Err "Error writing to temporary file.")
            in
            ( { model | tempFile = Just fileRef }, Cmd.fileSystem msg )

        TempFileCreated (Err err) ->
            ( { model | errorMsg = Just err }, Cmd.none )
```

In the `CreateTempFile` message, we use the `fileSelect` command to prompt the user to select a location for the temporary file. This will trigger the `TempFileCreated` message with the result of the file selection. If successful, we use the `File.System.write` function to write some text to the file and update our model accordingly.

## Deep Dive

Under the hood, the `File.System` module uses the browser's native `File` and `FileWriter` APIs to create the temporary file. It also takes care of file permissions and ensures that the file is properly deleted once it is no longer needed.

It's important to note that temporary files can only be created on the client-side, so this functionality will not work in server-side Elm applications.

## See Also

- [Elm File Package](https://package.elm-lang.org/packages/elm/file/latest/)
- [File System API](https://developer.mozilla.org/en-US/docs/Web/API/File_System_API)