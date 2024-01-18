---
title:                "Checking if a directory exists"
html_title:           "Lua recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Lua means verifying if a particular folder or directory is present in the file system. Programmers do this to avoid any errors or unexpected behavior that may occur if the code is trying to access a non-existing directory.

## How to:

To check if a directory exists in Lua, we can use the function `lfs.attributes()`. This function takes in a path as its argument and returns a table containing various attributes of the file or directory. To check for the existence of the directory, we can use the `mode` property of the table and see if it returns `directory` or not.

```
local lfs = require("lfs")
local directoryName = "/path/to/your/directory"
local attributes = lfs.attributes(directoryName)
if attributes and attributes.mode == "directory" then
  print("Directory exists!")
else
  print("Directory does not exist!")
end
```

The above code first requires the `lfs` library, which provides us with file system-related functions. Then, we specify the path of the directory we want to check for existence in the `directoryName` variable. Next, we use the `lfs.attributes()` function on the directory and store the result in a table called `attributes`. Finally, we use an `if` statement to check if the `attributes` table exists and if the `mode` property of the table is set to `directory`.

## Deep Dive:

Historically, checking if a directory exists was done by using the `os.execute()` function to execute a shell command and checking the return value. However, this approach has been deprecated and the recommended way now is to use `lfs.attributes()`.

There are a few alternatives for checking if a directory exists, such as using the `io.open()` function and checking for any errors, or using the `os.execute()` function to execute a `dir` command and parsing the output. However, these approaches are not recommended as they may not work on all platforms and can have security concerns.

When using `lfs.attributes()`, it is important to note that it returns `nil` if the path does not exist, or if there are any permission issues. So, it is necessary to check for the `attributes` table before accessing its properties.

## See Also:

- [Lua lfs Library Documentation](https://keplerproject.github.io/luafilesystem/manual.html)
- [Lua i/o Library Documentation](https://www.lua.org/pil/21.2.html)
- [Lua os Library Documentation](https://www.lua.org/manual/5.3/manual.html#6.9)