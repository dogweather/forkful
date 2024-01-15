---
title:                "Reading command line arguments"
html_title:           "Rust recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why
So, you've just started learning Rust and you're wondering why you would need to know how to read command line arguments. Well, first of all, command line arguments are used to pass information to a program at the time of its execution. This can be useful for providing input or options to your program, making it more dynamic and versatile.

##How To
Reading command line arguments in Rust is fairly simple. Let's take a look at an example using a simple program that takes in a name as a command line argument and prints out a greeting.

```
Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect(); // collect command line arguments into a vector

    // check if an argument was provided
    if args.len() > 1 {
        let name = &args[1]; // extract the first argument
        println!("Hello, {}!", name);
    } else {
        println!("Please provide a name as an argument.");
    }
}
```

Now, if we were to run this program with "John" as the argument, we would get the following output:

```
$ ./program John
Hello, John!
```

We can also pass in multiple arguments, as long as we specify which one we want to use in our code. For example, if we wanted to pass in a first and last name, we could do so like this:

```
$ ./program John Doe
Hello, John!
```

Note that when multiple arguments are passed in, they are separated by spaces.

##Deep Dive
Now that we've seen how simple it is to read command line arguments in Rust, let's take a deeper look at what's happening in our code. The ```env::args()``` function returns an iterator over the command line arguments, which we then collect into a vector. This vector can be accessed using ```args[index]``` where index represents the position of the argument. In our example, we used ```args[1]``` because the first argument (index 0) is the name of our program. We then used the ```&``` symbol to create a reference to the argument, as Rust doesn't allow us to pass values by reference without using this symbol.

##See Also
- [Rust documentation on command line arguments](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rust programming language homepage](https://www.rust-lang.org/)
- [Command line arguments in other programming languages](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)

That's it! You now have a basic understanding of how to read command line arguments in Rust. Happy coding!