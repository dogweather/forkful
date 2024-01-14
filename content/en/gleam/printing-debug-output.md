---
title:    "Gleam recipe: Printing debug output"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debug output is an essential tool for any programmer, as it allows for easier troubleshooting and understanding of code flow. By printing out the value of certain variables or function outputs, we can better understand what is happening behind the scenes in our programs. In this blog post, we will explore how to effectively use debug output in Gleam programming.

## How To
To print debug output in Gleam, we can use the `io.fmt` module. Let's take a look at an example:

```
Gleam
// Import the io.fmt module
import io.fmt

// Define a function that takes in a string parameter
pub fn greet(name: String) {
    // Use `fmt.println` to print a greeting message
    fmt.println("Hello, " ++ name)
}

// Call the `greet` function with "John" as the argument
greet("John")
```

After compiling and running this code, we will see the following output:

```
Hello, John
```

We can also print out the value of a variable by using `fmt.println` and the `debug` function from the `strings` module. Let's modify our previous example to include a variable:

```
Gleam
import io.fmt
import strings

pub fn greet(name: String) {
    // Use `debug` from the `strings` module to get the value of `name`
    let greeting = "Hello, " ++ name
    println(debug(greeting))
}

greet("John")
```

The output of this code will be:

```
"Hello, John"
```

## Deep Dive
In Gleam, we can also use debug output to print out the values of custom types. This can be achieved by implementing the `fmt::Debug` trait for our custom types. Let's see an example:

```
Gleam
import io.fmt

// Define a custom type called `Person`
type Person(name: String, age: Int)

// Implement the `fmt::Debug` trait for `Person`
impl Pp for Person {
    // Define the `pp` function to print out the values of `name` and `age`
    fn pp(self) {
        fmt.println(self.name)
        fmt.println(self.age)
    }
}

// Create a new `Person` instance
let john = Person("John", 30)

// Print out the values of `john`
println(debug(john))
```

The output of this code will be:

```
"John"
30
```

As we can see, by implementing the `fmt::Debug` trait, we can print out the values of our custom types without explicitly defining how they should be formatted.

## See Also
- Official Gleam documentation on debugging: https://gleam.run/book/tour/debugging.html
- An in-depth article on the importance of debug output: https://www.techrepublic.com/article/why-debug-output-is-vital-to-your-code/
- Another blog post on how to effectively use debug output in Python: https://realpython.com/python-debugging-pdb/

[John on Twitter](https://twitter.com/johnsmith)