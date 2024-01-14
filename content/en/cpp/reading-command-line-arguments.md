---
title:                "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Have you ever seen a program with multiple options that can be specified when running it from the command line? Have you ever wondered how these options are read and processed? In this blog post, we will explore the world of reading command line arguments in C++.

## How To

Reading command line arguments in C++ is a simple and useful skill to have. Let's say we want to create a program that can take in a user's name and age as command line arguments, and then print out a personalized message. To do this, we first need to include the "iostream" and "string" libraries. Then, we can use the "int main(int argc, char* argv[])" function to read the arguments passed in from the command line.

Next, we can use the "argc" and "argv" variables to access the arguments. "argc" holds the number of arguments entered, while "argv" is an array that holds each argument as a C-style string. So, in our example, if the user enters "John 25" as command line arguments, "argc" will have a value of 3 and "argv" will contain the strings "John" and "25" in the first and second indexes respectively.

Now, let's see the code in action:

```C++
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
  // We check to make sure there are enough arguments
  if (argc < 3) {
    std::cout << "Not enough arguments were provided!" << std::endl;
    return 0;
  }

  // Accessing the arguments
  std::string name = argv[1];
  std::string age = argv[2];

  // Printing out the personalized message
  std::cout << "Hello " << name << "! You are " << age << " years old." << std::endl;

  return 0;
}
```

**Output:**

```
$ ./program John 25
Hello John! You are 25 years old.
```

## Deep Dive

One thing to note is that command line arguments are always read in as strings. This means that if we want to use the arguments as other data types, such as integers or booleans, we need to do some conversion. For example, to convert the "age" argument to an integer, we can use the "stoi()" function from the "string" library. Similarly, we can use the "stof()" function to convert the argument to a float and "stod()" to convert it to a double.

Another useful tip is to use "getopt" to handle more complex command line options, such as options with values or options that can be used multiple times. "getopt" is a library that allows us to handle command line options in a more structured and organized way, making our code easier to read and maintain.

## See Also

- [The Basics of Command Line Arguments in C++ (GeeksforGeeks)](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Converting Strings to Numbers in C++ (Stack Overflow)](https://stackoverflow.com/questions/42388464/converting-string-to-int-in-c)
- [Using getopt in C++ (Linux Journal)](https://www.linuxjournal.com/article/5481)