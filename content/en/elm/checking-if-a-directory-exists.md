---
title:                "Checking if a directory exists"
html_title:           "Elm recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is a crucial step in any file handling operation. Whether you want to create a new directory, move files, or delete them, it is important to first confirm if the directory exists or not. This not only ensures the smooth functioning of your code but also prevents any potential errors or data loss.

## How To

To check if a directory exists in Elm, we can use the `exists` function from the `Elm.IO.Directory` module. This function takes in a relative or absolute path to the directory as an argument and returns a `Result Error Bool` type.

```
Elm.IO.Directory.exists "/path/to/directory"
--> Ok True
```

In the above example, the `exists` function returns `Ok True` indicating that the directory does exist. Let's look at another example where the directory does not exist.

```
Elm.IO.Directory.exists "/path/to/nonexisting/directory"
--> Ok False
```

Here, the function still returns `Ok False` but this time it indicates that the directory does not exist. It is important to handle this `Result` type in your code to handle potential errors or successful results.

## Deep Dive

Behind the scenes, the `exists` function uses the `withDirectoryEntry` function which takes care of opening and closing the directory entry. This ensures efficient and safe handling of the directory. If the directory path is a relative one, it will be resolved based on the directory where the program is executed.

A different approach to checking if a directory exists would be using the `getDir` function which returns a `Result Error Directory` type. This function also takes in the path to the directory as an argument but instead of returning a `Bool`, it returns a `Directory` type representing the directory. If the directory does not exist, the `Result` will contain an error message.

```
Elm.IO.Directory.getDir "/path/to/directory"
--> Ok <Directory>
```

See Also

- [Elm.IO.Directory module](https://package.elm-lang.org/packages/elm/file/latest/Elm-IO-Directory)
- [Elm Docs on File and Directory handling](https://guide.elm-lang.org/interop/file_system.html)