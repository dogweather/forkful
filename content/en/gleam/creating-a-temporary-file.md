---
title:    "Gleam recipe: Creating a temporary file"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files is a common practice in programming, especially when dealing with large or complex data that needs to be stored temporarily. These files are typically used for input/output operations or temporary caching of data.

## How To
To create a temporary file in Gleam, we can use the `temp_file` function from the `gleam_temp` module. This function takes in a file name and returns a temporary file handle. Let's see an example of how we can create a temporary file and write data to it:

```Gleam
import gleam/temp

fn write_to_temp_file() {
  let mut temp = temp_file("data.txt")
  temp.write("Hello, world!")
}
```

The above code will create a temporary file named `data.txt` and write the string "Hello, world!" to it. We can also specify the location where we want the temporary file to be created, by passing in a second argument to the `temp_file` function.

Once we are done using the temporary file, we can close it using the `close` function from the `gleam_temp` module. This will delete the temporary file from the system.

## Deep Dive
Under the hood, the `temp_file` function creates a file in the system's temporary directory. This location can vary depending on the operating system and environment variables. The temporary file created will have a unique name to avoid conflicts with other temporary files.

It is important to note that temporary files should not be relied upon for permanent data storage as their lifespan is unpredictable and they can be deleted at any time. They should only be used for temporary storing and processing of data.

## See Also
- [The `gleam_temp` module](https://gleam.run/documentation/stdlib/gleam_temp/)
- [Using temporary files in Rust](https://doc.rust-lang.org/std/fs/struct.File.html)