---
title:    "Elm recipe: Writing a text file"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a simple task, but it is an important aspect of programming. Text files allow us to store and access data in a structured and readable manner, making it easier to work with large amounts of information.

## How To

To write a text file in Elm, we first need to import the `File` and `Text.Encoding` modules. Then, we can use the `File.encode` function to convert our data into a `Text` value, and finally use the `File.writeFile` function to write the text file.

```
import File
import Text.Encoding

main : Program Never Model
main =
  let
    data = "This is sample text data."
    textValue = Text.Encoding.encode data
  in
    File.writeFile "output.txt" textValue
```

Running this code will create a file named "output.txt" in the same directory as our Elm program. The contents of the file will be our data string.

## Deep Dive

While writing a simple text file is straightforward, there are several things to keep in mind when working with text files in Elm. Here are a few tips to help you get started:

- Use `Text.Encoding.encode` to convert your data into a `Text` value before writing it to a file. This ensures that special characters are properly encoded.
- Always specify the file path and name when using `File.writeFile`. If you don't, the file will be saved in a temporary location and may be deleted by the system.
- To append data to an existing text file, use `File.appendFile` instead of `File.writeFile`.
- If you need to read a text file, use the `File.read` function to get a `Task` value, which can then be handled in your `update` function.

For more detailed information on reading and writing text files, check out the Elm documentation on the `File` module.

## See Also

- Elm Documentation on the `File` Module: https://package.elm-lang.org/packages/elm/file/latest/
- Guide to Working with Text Files in Elm: https://www.kirupa.com/html5/reading_writing_text_files_elm.htm