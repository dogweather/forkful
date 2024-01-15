---
title:                "ディレクトリが存在するか確認する"
html_title:           "TypeScript: ディレクトリが存在するか確認する"
simple_title:         "ディレクトリが存在するか確認する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Why
Why would someone want to check if a directory exists in their TypeScript code? Directory checking is important for ensuring that the code is properly accessing and manipulating files within a project. It also helps prevent any errors or unexpected behavior that may occur if a directory is missing.

# How To
```TypeScript
import * as fs from 'fs';

// Method 1: using fs.existsSync()
if (fs.existsSync('directory/path')) {
    // Code to handle if directory exists
    console.log("Directory exists!");
} else {
    // Code to handle if directory does not exist
    console.log("Directory does not exist!");
}

// Method 2: using fs.statSync()
try {
    // Check if statSync throws error
    fs.statSync('directory/path');
    // Code to handle if directory exists
    console.log("Directory exists!");
} catch (err) {
    // Code to handle if directory does not exist
    console.log("Directory does not exist!");
}
```

Sample Output:
```
Directory exists! // When directory exists at given path
Directory does not exist! // When directory does not exist at given path
```

# Deep Dive
In the first method, `fs.existsSync()` returns a boolean value, `true` if the directory exists and `false` if it does not. It does not throw an error if the path given is a regular file instead of a directory. However, this method is deprecated and exists for legacy purposes.

In the second method, `fs.statSync()` checks the status of the file at the given path and returns an instance of `fs.Stats`. A file is considered a directory if its `Stats` object has the `isDirectory()` method. This method throws an error if the path given is not valid, including if the directory does not exist.

# See Also
- Node.js `fs` Module Documentation: https://nodejs.org/api/fs.html
- TypeScript `import` Statement Documentation: https://www.typescriptlang.org/docs/handbook/modules.html#importing-a-module-using-a-mnemonic
- `fs.statSync()` Documentation: https://nodejs.org/api/fs.html#fs_fs_statsync_path_options
- `fs.existsSync()` Documentation: https://nodejs.org/api/fs.html#fs_fs_existssync_path