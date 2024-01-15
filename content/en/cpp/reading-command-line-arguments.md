---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to C++, you may have heard of "command line arguments," but you might not be quite sure what they are or why you would need to use them in your code. Simply put, command line arguments allow you to provide input to your program from the command line, making your programs more flexible and customizable.

## How To

To use command line arguments in your C++ program, you first need to include the `iostream` and `string` libraries:

```C++
#include <iostream>
#include <string>
```

Then, in your `main` function, you can declare two parameters: `int argc` and `char* argv[]`. These parameters represent the number of arguments passed in and an array of strings containing the arguments, respectively.

```C++
int main(int argc, char* argv[]) {
  // code goes here
}
```

To access the arguments, you can use the `argv` array, where `argv[0]` represents the name of the program and `argv[1...]` represents any additional arguments passed in. For example, if you were to run your program with the command `./program arg1 arg2`, `argv[1]` would be equal to "arg1" and `argv[2]` would be equal to "arg2".

You can also use the `argc` parameter to determine how many arguments were passed in and use conditional statements to handle different scenarios. For example, if `argc` is equal to 3, you know that two arguments were passed in and you can perform a specific action based on that.

Now, let's see an example of using command line arguments in a program. Let's say we want to create a simple calculator that takes in two numbers and performs a basic mathematical operation on them. We can use command line arguments to specify the numbers and the operation to be performed.

```C++
#include <iostream>
#include <string>
using namespace std;

int main(int argc, char* argv[]) {
  // check if correct number of arguments were passed in
  if (argc != 4) {
    cout << "Usage: ./calculator num1 num2 operation\n";
    return 1;
  }

  // convert arguments to floats
  float num1 = stof(argv[1]);
  float num2 = stof(argv[2]);

  // perform specified operation
  if (strcmp(argv[3], "add") == 0)
    cout << num1 + num2 << endl;
  else if (strcmp(argv[3], "subtract") == 0)
    cout << num1 - num2 << endl;
  else if (strcmp(argv[3], "multiply") == 0)
    cout << num1 * num2 << endl;
  else if (strcmp(argv[3], "divide") == 0)
    cout << num1 / num2 << endl;
  else
    cout << "Invalid operation\n";

  return 0;
}
```

And here's a sample output when running this program with different arguments:

```
./calculator 5 3 add
8

./calculator 10 4 subtract
6

./calculator 2 8 multiply
16

./calculator 25 5 divide
5
```

## Deep Dive

Command line arguments can also be used to pass in more complex data such as file paths, flags, or multiple values. You can also use libraries such as `boost::program_options` to parse and handle more complicated input from the command line.

One important thing to keep in mind when using command line arguments is to make sure you properly handle any potential errors, especially with user input. It's always a good idea to include usage instructions or error messages when the wrong number of arguments are passed in or when an invalid input is provided.

## See Also

Here are some additional resources to learn more about reading command line arguments in C++:

- [C++ Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C++ Program Options Library](https://www.boost.org/doc/libs/1_72_0/doc/html/program_options.html)
- [Handling Command Line Arguments in C++](https://www.codingame.com/playgrounds/14213/handling-command-line-arguments-in-c)