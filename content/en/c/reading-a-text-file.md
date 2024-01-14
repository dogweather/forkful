---
title:    "C recipe: Reading a text file"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Reading a text file is a fundamental task in any programming language, including C. It allows us to access and manipulate the contents of a file, which is essential for many programs. Understanding how to read a text file in C can greatly increase your efficiency and productivity as a programmer.

## How To

To read a text file in C, we will first need a *FILE* pointer, which is a special type of variable used to store information about a file. We can declare it using the *fopen()* function, which takes two parameters: the name of the file and the mode in which we want to open it. The mode "r" indicates that we want to open the file for reading.

```C
FILE *fp;
fp = fopen("text.txt", "r");
```

Next, we can use the *fscanf()* function to read data from the file. This function works similarly to the *scanf()* function used for reading input from the console, except that it takes an additional parameter, the file pointer.

```C
int num;
fscanf(fp, "%d", &num);
```

We can also use a *while* loop to iterate through the file until we reach the end, using the *feof()* function to check for the end of the file.

```C
while(!feof(fp)){
    char str[100];
    fgets(str, 100, fp); // read a line from the file
    printf("%s", str); // print the line to the console
}
```

To close the file, we use the *fclose()* function, which takes the file pointer as a parameter.

```C
fclose(fp);
```

The sample code above reads an integer from the file and then prints out all the lines until the end of the file is reached. You can try it out by creating a *text.txt* file with some numbers and strings in it.

**Sample output:**

```
123
Hello
World
```

## Deep Dive

In C, a text file is a sequence of characters stored in a file on the disk. To read the file, we need to specify the path to the file, which can either be the absolute path or the relative path to the current working directory. The file must also exist in that path, or else an error will occur.

Text files in C are treated as streams of characters, which means that we can manipulate them just like we do with input from the console. For example, we can use the *sscanf()* function to read data from a string, which is useful for parsing data from a file with a specific format.

Another essential concept to understand when reading a text file in C is *buffering*. When we read a file, the data is not read directly from the disk but is instead buffered in memory. This allows for faster reading and manipulation of the data. However, we need to be careful when modifying the file in the program, as the changes will not be reflected in the file until it is closed and reopened.

## See Also

- [fopen() function](https://www.geeksforgeeks.org/fopen-for-an-existing-file-in-write-mode/)
- [fscanf() function](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)
- [Buffering in C](https://www.geeksforgeeks.org/buffering-in-c/)
- [sscanf() function](https://www.geeksforgeeks.org/sscanf-in-c/)
- [File Input/Output in C](https://www.programiz.com/c-programming/c-file-input-output)