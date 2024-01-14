---
title:                "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why 

Reading a text file is a common task for many programmers, and it can serve a variety of purposes. For example, you may need to extract data from a log file, import data into a database, or simply analyze the contents of a text file. Whatever your reason may be, learning how to read a text file using Go can improve your skills as a programmer and make your tasks more efficient.

## How To 

To start, you will need to open the text file using the ```os``` package and the ```Open()``` method. This method takes in two arguments: the path to the file and the mode in which you want to open it. Once you have opened the file, you will need to create a ```Scanner``` to read through its contents line by line. This can be done using the ```bufio``` package and the ```NewScanner()``` method. Lastly, you will need to use a ```for``` loop to iterate through each line and perform any desired actions.

Here's a simple code example of how to read a text file and print each line to the console:

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Open file
	file, err := os.Open("example.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()
	
	// Create scanner
	scanner := bufio.NewScanner(file)
	
	// Read and print each line
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Println(line)
	}
}
```

Sample output:

```
This is the first line of the text file.
And this is the second line.
The third line is here!
```

By using the ```defer``` keyword, we ensure that the file is closed after we are done using it. Additionally, we use the ```Text()``` method to extract the current line from the scanner. For more advanced operations on text files, you can use other methods such as ```Scan()```, ```Bytes()```, or ```Runes()```.

## Deep Dive

There are many other aspects to consider when reading a text file in Go, such as handling errors, managing file permissions, and dealing with large files. It is important to use good programming practices, such as checking for errors and closing files properly, to avoid potential issues.

In addition, Go provides some helpful tools for working with text files. For example, the ```ioutil``` package has a ```ReadFile()``` method that allows you to read the entire contents of a file at once, which can be useful for smaller files. You can also use the ```os``` package's ```Stat()``` method to get information about the file, such as its size and modification time.

Another useful tip is to use function literals or anonymous functions when performing actions on each line. This allows you to customize the actions you want to perform without having to create separate functions for each one.

## See Also

- [The official Go documentation on the ```os``` package](https://golang.org/pkg/os/)
- [A thorough tutorial on working with text files in Go](https://www.callicoder.com/golang-read-file-open-read-write-file/)
- [In-depth explanation of function literals and anonymous functions in Go](https://gobyexample.com/anonymous-functions)

Reading text files is just one of the many file operations that you can perform with Go. By understanding how to handle text files, you'll have a strong foundation for working with other types of files as well. Happy coding!