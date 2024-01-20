---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the process of extracting parameters provided to your application by the user. This allows you to change your program's behavior based on user's inputs.

## How to:

In Gleam, we utilize the `gleam/otp` library to read command line arguments from inside a `start` function. Here's a simple example:

```Gleam
import gleam/otp.{Application}

fn start() {
    let args = Application.get_args() // Fetch the command line arguments
    case args {
        Ok(arg_values) -> 
          // Extracts the list of argument values
          io.println(arg_values)
        Error ->
          io.println("No arguments found")
    }
}
```

This will print out the command line arguments used on starting the application. If there's no command line arguments, it will print "No arguments found". 

## Deep Dive

Back in the days of UNIX systems, command line arguments were the first and easiest way to allow interactivity with programs. That's why they're a staple in most modern programming languages.

There are some alternatives to command line arguments. You can use standard input, which lets your program accept user input while it is running. Or you could use environment variables to have a set of inputs that are present throughout the program's execution.

In Gleam, the `Application.get_args()` function fetches command line arguments as a Result type. If no command line arguments are given, the Result type will be an `Error`. It's your job to handle this error gracefully! 

## See Also

To learn more about Gleam, visit its [official website](https://gleam.run/). For more detailed information about the `gleam/otp` library, check out [their documentation](https://hexdocs.pm/gleam_otp/readme.html). To see examples of how command line arguments can be used in real-world programs, check out this [Gleam Cookbook](https://github.com/gleam-lang/otp).