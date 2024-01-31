---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in PHP is about saving data to a file on the server. Programmers often do this for data logging, configuration settings, or exporting data that's meant to be read by humans or other systems.

## How to:

Writing to a file in PHP can be as simple as using the `file_put_contents()` function, which takes a filename and a string of data. Here's a quick example:

```php
<?php
$data = "Hello, world!\n";
file_put_contents("example.txt", $data);
?>
```

Running this script creates "example.txt" with the content "Hello, world!".

For more control, you can open a file, write to it, then close it:

```php
<?php
$file = fopen("example.txt", "w") or die("Unable to open file!");
$txt = "Hello again, world!\n";
fwrite($file, $txt);
fclose($file);
?>
```

Both scripts result in the same output in "example.txt".

## Deep Dive

Historically, PHP's `fopen()`, `fwrite()`, and `fclose()` provided detailed control for file writing operations, like appending or locking. `file_put_contents()` was introduced in PHP 5 for a simplified approach.

Alternatives include using `fputcsv()` for generating CSV files or the `SplFileObject` class for object-oriented file operations. Implementation details include handling file permissions and ensuring exception handling or error checking with `or die()` or `try-catch` blocks.

## See Also

- [PHP file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP fopen()](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [PHP file handling](https://www.php.net/manual/en/book.filesystem.php)
- [Understanding file permissions](https://www.php.net/manual/en/function.chmod.php)
