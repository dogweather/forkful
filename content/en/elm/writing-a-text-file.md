---
title:                "Elm recipe: Writing a text file"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a basic but important tool in any programming language, and that includes Elm. Writing text files allows you to store information in a format that is easily readable and editable by both humans and computers. With Elm, you can easily create and manipulate text files to suit your needs, whether it's for storing data or producing output for your application.

## How To

To create a text file in Elm, you will need to use the `elm/file` package. First, import the `File` and `Bytes` modules:

```Elm
import File
import Bytes
```

Next, you will need to define a `File` type in your model, which will be responsible for storing the file's data:

```Elm
type alias Model =
    { file : File.File
    ...
    }
```

To create a new file, use the `File.new` function, passing in the file name and data (in bytes) as arguments. You can use the `Bytes.Encode.string` function to encode a string as bytes:

```Elm
newFile = File.new "example.txt" (Bytes.Encode.string "Hello world!")
```

To write data to an existing file, use the `File.write` function, passing in the file and data as arguments:

```Elm
writeFile = File.write existingFile (Bytes.Encode.string "New data!")
```

To read data from a file, use the `File.read` function. This function returns a `Task` that can be used to handle the file's data:

```Elm
readFile =
    Task.perform
        (\result ->
            case result of
                Ok data ->
                    -- Do something with the file data

                Err error ->
                    -- Handle the error
        )
        (File.read existingFile)
```

Once you have completed your operations on the file, you can save it by calling the `File.save` function:

```Elm
saveFile = File.save existingFile
```

## Deep Dive

Behind the scenes, the `elm/file` package utilizes the Browser's `File` and `Blob` APIs to handle the creation, reading, and writing of text files. This ensures that the package is cross-platform compatible and can be used in both web and desktop environments.

Additionally, the `File.listDirectory` function allows you to obtain a list of files from a specified directory, and the `File.delete` function can be used to remove a file from the system.

## See Also

For more information on the `elm/file` package and other useful tools for working with files in Elm, check out the following links:

- [Elm File package documentation](https://package.elm-lang.org/packages/elm/file/latest/)
- [Guide to File IO in Elm](https://dev.to/marvs/file-io-in-elm-caf)
- [Elm Bytes library documentation](https://package.elm-lang.org/packages/elm/bytes/latest/)
- [Elm File Example](https://github.com/jxxcarlson/elm-file-example)