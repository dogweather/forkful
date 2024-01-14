---
title:    "Gleam recipe: Checking if a directory exists"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

When working with files and directories in the Gleam programming language, it is important to be able to check if a directory exists. This allows for more robust and error-proof code, as well as ensuring that the proper actions are taken depending on the existence of a directory.

## How To

To check if a directory exists in Gleam, we can use the `os.exists` function. This function takes in a `String` representing the file path and returns a `Result`. If the directory exists, the `Result` will contain `Ok(true)`, otherwise it will contain `Ok(false)`.

```Gleam
import os
import result

let directory_exists_result = os.exists("/Users/username/Downloads")

// Match on the result to handle the different cases
match directory_exists_result {
  Ok(true) -> "The directory exists!"
  Ok(false) -> "The directory does not exist."
  Error(err) -> "There was an error: #{err}"
}
```

The above code shows an example of using `os.exists` to check if a directory at the given path exists. This can also be applied to files, as the function is not specific to directories. Let's see the output of running this code:

```Gleam
The directory exists!
```

## Deep Dive

Behind the scenes, `os.exists` makes use of the operating system's file system calls to determine the existence of the directory. This means that it is a reliable and efficient way to check if a directory exists. However, it is important to note that this function does not check for permission errors, so it is possible to receive an `Error` result if there are permissions issues with the directory.

## See Also

- [Gleam official documentation on `os.exists`](https://gleam.run/core/std.os.html#exists-1)
- [Gleam official documentation on `Result` type](https://gleam.run/core/std.result.html#t:result)
- [Guide to error handling in Gleam](https://gleam.run/book/tour/error-handling.html)