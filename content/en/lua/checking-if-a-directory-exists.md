---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Checking If a Directory Exists in Lua

## What & Why?
Checking if a directory exists is a method of verifying the presence of a particular directory on a file system before running specific operations. Programmers do this to avoid errors when trying to access, modify, or delete directories.

## How to:
In Lua, you can check if a directory exists using the `lfs` module. The code is simple:

```Lua
lfs = require('lfs')

function isDir(path)
    if lfs.attributes(path, 'mode') == 'directory' then
        return true
    else
        return false
    end
end

print(isDir('/path/to/directory'))   -- Replace with your directory
```

Here, `/path/to/directory` is the directory you want to check. If it exists, this script will print `true`. If it doesn’t, it will print `false`.

## Deep Dive
Historically, verifying a directory’s existence hasn’t always been straightforward in Lua. Unlike some other languages, Lua does not have built-in functions supporting this out of the box. Therefore, the Lua File System (`lfs`) module is used for such processes.

A popular alternative to the `lfs` module approach is executing OS commands within Lua scripts. This is a powerful but dangerous method, as it brings in inherent OS vulnerabilities. Lua developers recommend sticking with the `lfs` module to maintain control over what happens.

Under the hood, `lfs.attributes(path, 'mode')` retrieves the `mode` attribute of the file/directory at the given `path`. If this `mode` is 'directory', the function returns `true`, indicating that a directory exists at the given `path`.

## See Also
2. [Lua `os` Library](https://www.lua.org/manual/5.3/manual.html#6.9) (Remember, it's powerful but use with caution!)
3. [Tutorial on Manipulating Files and Directories in Lua](https://www.tutorialspoint.com/lua/lua_file_io.htm)