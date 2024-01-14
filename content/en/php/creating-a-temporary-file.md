---
title:    "PHP recipe: Creating a temporary file"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Creating temporary files is a core function in many programming languages, including PHP. These temporary files serve as temporary storage for data or as a placeholder for information that will be used and then discarded. They are important for optimizing memory usage and ensuring efficient allocation of resources.

## How To
Creating a temporary file in PHP is a straightforward process. In this example, we will use the `tempnam()` function, which generates a unique temporary filename.

```
<?php
// specify the directory where the file will be created
$tempDirectory = '/tmp';

// create the temporary file and store the filename in a variable
$file = tempnam($tempDirectory, 'tmp_');

// write some content to the file
$output = "This is the content of our temporary file.";

// open the file for writing
$handle = fopen($file, 'w');

// write the output to the file
fwrite($handle, $output);

// close the file
fclose($handle);

// output the temporary file's name
echo "Temporary file created: " . $file;
```

The output of this code would be:

```
Temporary file created: /tmp/tmp_230418374
```

You can also use the `tmpfile()` function to create a temporary file without specifying a directory. This function will automatically create and open the file for you, and return a file handle that you can use to write to the file.

```
<?php
// create a temporary file
$file = tmpfile();

// write some content to the file
$output = "This is the content of our temporary file.";

// write the output to the file
fwrite($file, $output);

// output the temporary file's name
echo "Temporary file created: " . stream_get_meta_data($file)['uri'];
```

The output of this code would be:

```
Temporary file created: /var/folders/57/m8m7rly92vd9zwcli8dftpw00000gn/T/phpdPEAw0.tmp
```

## Deep Dive
Behind the scenes, the `tempnam()` function creates an empty file with a random, unique filename. It then combines the specified directory and the prefix to create the full temporary file path. This allows you to specify a custom directory and prefix for your temporary files.

The `tmpfile()` function, on the other hand, creates a file in the system's default temporary directory with a unique filename, and opens it for writing. This eliminates the need for specifying a directory or generating a unique filename manually.

It's important to note that both `tempnam()` and `tmpfile()` functions will create files with default permissions, which may not be suitable for your needs. You can use the `chmod()` function to change the permissions of the temporary file as needed.

## See Also
- [PHP Manual: tempnam() function](https://www.php.net/manual/en/function.tempnam.php)
- [PHP Manual: tmpfile() function](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP Manual: chmod() function](https://www.php.net/manual/en/function.chmod.php)