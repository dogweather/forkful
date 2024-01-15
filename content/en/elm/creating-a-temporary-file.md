---
title:                "Creating a temporary file"
html_title:           "Elm recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Ever found yourself in need of creating a temporary file in your Elm program? Temporary files can be useful for various reasons such as storing information that may only be needed temporarily or for testing purposes. Whatever your reason may be, this article will show you how to easily create a temporary file in Elm.

## How To

To create a temporary file in Elm, we will be utilizing the `File` and `Task` modules. First, we need to import both of these modules into our program:

```Elm
import File
import Task
```

Next, we will use the `File.tempFile` function to create the temporary file. This function takes in two arguments: a directory path and a file name. Let's say we want to create a temporary file called `my_temp_file.txt` in our project's root directory. We can achieve this by calling the `File.tempFile` function with the following arguments:

```Elm
let
  dirPath = "./"
  fileName = "my_temp_file.txt"
  result = Task.perform fileCreated (File.tempFile dirPath fileName)
in
  text "File created!"
```

We have used the `Task.perform` function to handle the asynchronous nature of creating a file. The `fileCreated` function will be called once the file is successfully created. We can define this function as follows:

```Elm
fileCreated : Result File.Error FilePath -> Html msg
fileCreated result =
  case result of
    Ok path ->
      text ("Temporary file created at: " ++ path)

    Err error ->
      text ("File creation failed with error: " ++ (toString error))
```

This code will display a message informing us of the successful creation of our temporary file. We can also use the `File.write` function to write content to our temporary file. Here's an example of writing the string "Hello, World!" to our temporary file:

```Elm
let
  dirPath = "./"
  fileName = "my_temp_file.txt"
  content = "Hello, World!"
  result = Task.perform fileWritten (File.tempFile dirPath fileName)
in
  text "Content written to file!"
```

Notice that we have used a different function, `fileWritten`, for handling the result of our `File.write` operation. This function is defined similarly to `fileCreated` but with one small difference:

```Elm
fileWritten : Result File.Error FilePath -> Html msg
fileWritten result =
  case result of
    Ok path ->
      Task.perform fileClosed (File.write path content)

    Err error ->
      text ("Write failed with error: " ++ (toString error))
```

Here, we have called the `Task.perform` function once again, this time to handle the result of the `File.write` operation. Finally, our `fileClosed` function would close the file once the content is successfully written:

```Elm
fileClosed : Result File.Error () -> Html msg
fileClosed result =
  case result of
    Ok _ ->
      text "Temporary file closed!"

    Err error ->
      text ("File closing failed with error: " ++ (toString error))
```

And that's it! We have successfully created, written content to, and closed our temporary file in Elm.

## Deep Dive

Behind the scenes, the `File.tempFile` function is using the browser's `File` API to create the temporary file. This API creates a file with a unique name in the specified directory. So, if we try to create a file with the same name in the same directory, it will generate a new unique name for the file. This ensures that our temporary files do not conflict with each other and are always distinguishable.

## See Also

- [Official Elm documentation on File module](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Official Elm documentation on Task module](https://package.elm-lang.org/packages/elm/core/latest/Task)