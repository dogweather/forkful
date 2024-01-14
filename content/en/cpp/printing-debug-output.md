---
title:    "C++ recipe: Printing debug output"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the coding process. It allows developers to identify and fix errors in their code, ensuring that the final product runs smoothly. One way to assist in the debugging process is by printing out debug output. This involves displaying specific values or messages in the terminal or console while the program is running. But why exactly would someone want to engage in printing debug output?

The answer is simple: it helps with troubleshooting. By printing out relevant information, developers can track the flow of their program and identify any potential bugs or issues. It also allows for easier monitoring of variables and their values, providing more insight into the inner workings of the code.

## How To

To print debug output in C++, you can use the `cout` function from the `iostream` library. This allows you to output data to the terminal. Let's take a look at an example of printing out a simple message:

```C++
cout << "Debug output: Program is running..." << endl;
```

The `cout` keyword is followed by the message we want to print within quotation marks. The `endl` statement adds a line break for better readability.

We can also print out the value of a variable. Consider the following code:

```C++
int num = 5;
cout << "The value of num: " << num << endl;
```

This will print: "The value of num: 5" in the terminal.

But what if we want to display the value of a variable at different points in the program? We can use an `if` statement with `cout` inside, like so:

```C++
if(num == 5){
    cout << "The value of num is 5" << endl;
}else{
    cout << "The value of num is not 5" << endl;
}
```

This will print out one of the two messages depending on the value of `num`.

## Deep Dive

Now that we know the basics of printing debug output, let's delve into some other techniques that can come in handy.

One useful method is to create a custom debug output function. This will allow us to easily toggle debug output on and off, instead of having to comment out `cout` statements every time we want to remove them. Here's an example:

```C++
#include <iostream>
using namespace std;

//This is our custom debug output function
void debugPrint(string message){
    #ifdef DEBUG //only execute this code if DEBUG is defined
        cout << message << endl;
    #endif
}

int main(){
    int num = 5;

    //Some code here
    debugPrint("The value of num is: " + to_string(num)); //to_string converts int to string

    //More code here
    num++;
    debugPrint("Now the value of num is: " + to_string(num));

    return 0;
}

```

If we define the `DEBUG` constant at the top of our code, the debug output will be printed. Otherwise, it will be ignored.

Another helpful technique is using `cerr` instead of `cout`. `cerr` is similar to `cout` but is specifically designed for error messages and is usually displayed in a different color in the terminal. This can be useful when trying to differentiate between normal output and error messages.

## See Also

For more information on C++ output and debugging, check out these sources:

- [C++ Output using cout](https://www.geeksforgeeks.org/basic-input-output-c/)
- [Debugging Techniques in C++](https://www.programiz.com/cpp-programming/debugging)
- [Custom Debug Output Function](https://embeddedartistry.com/blog/2017/1/24/demystifying-c-cout-debugging-tricks)