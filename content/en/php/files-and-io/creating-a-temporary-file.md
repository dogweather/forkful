---
date: 2024-01-20 17:40:51.530064-07:00
description: "How to: PHP helps you create temporary files with the `tmpfile()` function,\
  \ which creates a file for you in your system's temp directory. Here's a quick\u2026"
lastmod: '2024-03-13T22:45:00.184521-06:00'
model: gpt-4-1106-preview
summary: PHP helps you create temporary files with the `tmpfile()` function, which
  creates a file for you in your system's temp directory.
title: Creating a temporary file
weight: 21
---

## How to:
PHP helps you create temporary files with the `tmpfile()` function, which creates a file for you in your system's temp directory. Here's a quick example:

```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Hello, temporary world!");
rewind($tempFile);

echo fread($tempFile, 1024); // Read what we wrote to the file

fclose($tempFile); // The temporary file is removed automatically
?>
```

Sample Output:
```
Hello, temporary world!
```

You can also use `tempnam()` to get a file name that you can manage yourself:

```PHP
<?php
$tempFilePath = tempnam(sys_get_temp_dir(), 'Tux');
file_put_contents($tempFilePath, "Penguins are cool!");

echo file_get_contents($tempFilePath); // Read the content

unlink($tempFilePath); // Delete the file when you're done
?>
```

Sample Output:
```
Penguins are cool!
```

## Deep Dive
The `tmpfile()` function has been in PHP since the early days. It handles file creation and cleanup for you, nicely sidestepping potential security risks of leaving sensitive data hanging around.

On the flip side, `tempnam()` gives you just a name, leaving the file management in your hands. One caveat: always remember to `unlink()` the file when you're done.

These temporary files are usually stored in your system's default temp directory, which you can find with `sys_get_temp_dir()`. This location can vary based on your operating system and environment configuration.

You also have alternatives like `tempnam()` and `tmpfile()`, and there's the fancier `sys_get_temp_dir()` for getting that elusive temp directory. But remember the golden rule with temporary files: tidy up after yourself—PHP does some of this automatically, but it's good practice to be explicit.

## See Also
- [The official PHP documentation for tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP manual on tempnam() function](https://www.php.net/manual/en/function.tempnam.php)
- [PHP.net's information on sys_get_temp_dir()](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [Filesystem Security](https://www.php.net/manual/en/security.filesystem.php)
