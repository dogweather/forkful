---
title:                "Reading a text file"
html_title:           "PHP recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file in the context of programming means opening and accessing the contents of a file that contains human-readable text. Programmers may need to read text files to extract information or manipulate data, among other reasons.

## How to:
To read a text file in PHP, you can use the fopen() function to open the file and the fgets() function to read each line. Here's an example of reading a file called "example.txt" that contains the words "Hello world" on the first line:

```PHP
$file = fopen("example.txt", "r"); // open the file in read-only mode
echo fgets($file); // output: Hello world
fclose($file); // close the file
```

You can also use a while loop with the feof() function to read the file line by line until the end of the file is reached:

```PHP
$file = fopen("example.txt", "r");
while (!feof($file)) { // continues until end of file is reached
    echo fgets($file) . "<br>"; // output each line with a line break 
}
fclose($file);
```

## Deep Dive:
Before the widespread use of computers, text files were used to store and share information, making them a popular format for data transfer. Today, alternatives such as CSV files or databases are often used for data storage and manipulation. However, text files still have their uses, such as in log files or configuration files.

When reading a text file, it's important to consider the file's encoding, which determines how the characters will be interpreted. PHP's fopen() function has an optional parameter for specifying the file's encoding. Additionally, the file must be readable by the PHP process running on the server.

## See Also:
- [fopen() function](https://www.php.net/manual/en/function.fopen.php)
- [fgets() function](https://www.php.net/manual/en/function.fgets.php)
- [feof() function](https://www.php.net/manual/en/function.feof.php)
- [Character encoding](https://www.w3schools.com/html/html_charset.asp)