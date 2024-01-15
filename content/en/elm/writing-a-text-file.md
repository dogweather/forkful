---
title:                "Writing a text file"
html_title:           "Elm recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Text files are an essential part of programming and are used for storage, data transfer, and communication between programs. In Elm, writing a text file can be a useful skill for tasks such as saving user input or exporting data in a human-readable format.

## How To
To write a text file in Elm, we first need to import the `File` module. We can then use the `write` function to create a file and write contents to it. Here's an example:

```Elm
import File

writeTextFile : List String -> Task x ()
writeTextFile lines =
    File.write "myFile.txt" (String.unlines lines)
```

In this code, we first import the `File` module which gives us access to the `write` function. We then define a function called `writeTextFile` which takes in a list of strings and returns a task (similar to a promise in JavaScript) with no specific type of error (`x`) and no return value (`()`). Inside the function, we use the `write` function to create a file named `myFile.txt` and write the contents of the list of strings to it using the `String.unlines` function to add line breaks between each element.

To actually execute this function and create the file, we can use the `run` function from the `Task` module. We can then pass in our `writeTextFile` function and a callback function to handle any potential errors:

```Elm
import Task

main : Program Never
main =
    Task.attempt (\_ -> Debug.log "File created") (File.run (writeTextFile ["Hello", "World"]))
```

In this example, we use the `Task` module to run our `writeTextFile` function and handle any errors by logging a message to the console. And that's it! Our file should now be created with the specified contents.

## Deep Dive
There are a few important things to note when writing a text file in Elm. First, the `write` function takes in two parameters: the file name (as a string) and the file contents (as another string). If the file already exists, it will be overwritten with the new contents.

Secondly, the task returned by the `write` function is asynchronous, which means we need to use the `Task` module to handle the callback function and potential errors. This is different from other synchronous functions in Elm like `Debug.log` or `String.length`, where we can simply call the function and get a result without using the `Task` module.

Lastly, it's important to handle potential errors when writing a text file, as the task will fail if there are any issues with creating or writing to the file. This can be done using the `Task.attempt` function, as shown in the example above.

See Also:
- Official Elm Language Guide on writing files: https://elm-lang.org/docs/interop/file
- Official Elm Package for working with files: https://package.elm-lang.org/packages/elm/file/latest/