---
title:    "Gleam recipe: Checking if a directory exists"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to check if a specific directory exists before performing a task? In Gleam, there are various scenarios where this would be necessary, such as ensuring a file is in the correct location before reading it. In this blog post, we will explore how to check if a directory exists in Gleam.

## How To

To check if a directory exists in Gleam, we will use the standard library function `file.exists/1`. This function takes a string representing a file path and returns a boolean value indicating if the file exists or not. Let's try it out with a hypothetical path of `src/utils`:

```Gleam
directory := "src/utils"

if file.exists(directory) {
    io.print("The directory exists!")
} else {
    io.print("The directory does not exist.")
}
```

The`file.exists/1` function also works for nested directories, so you can check for `src/utils/models`, for example.

```Gleam
directory := "src/utils/models"

if file.exists(directory) {
    io.print("The directory exists!")
} else {
    io.print("The directory does not exist.")
}
```

The output for both of these examples would be:

```
The directory exists!
```

## Deep Dive

Behind the scenes, the `file.exists/1` function uses the `file.metadata/1` function, which returns a `file_result` type. This type contains information about the file, including its existence. Through pattern matching, we can access this information and handle it accordingly.

For example, we can create a function that checks if a directory exists and returns a custom message depending on the result:

```Gleam
pub fn check_directory(directory) {
    case file.metadata(directory) {
        Ok(metadata) ->
            case metadata {
                File -> "This is a file, not a directory."
                Directory -> "${directory} exists."
            }
        Err(err) -> "${directory} does not exist."
    }
}

io.print(check_directory("src/utils"))
```

The output for this example would be:

```
src/utils exists.
```

## See Also

- [Official Gleam Documentation: Working with Files and Directories](https://gleam.run/articles/files-and-directories)
- [Gleam Standard Library: File module](https://github.com/gleam-lang/gleam_stdlib/blob/master/standard-library/file.md)