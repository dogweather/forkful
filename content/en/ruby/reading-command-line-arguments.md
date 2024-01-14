---
title:    "Ruby recipe: Reading command line arguments"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Command line arguments are a powerful tool in a programmer's arsenal. They allow us to pass information to our programs directly from the command line, making our scripts more user-friendly and versatile. In this blog post, we will explore the basics of reading command line arguments in Ruby.

## How To
To start, let's create a simple Ruby script that reads and outputs command line arguments. Open your favorite text editor and follow along!

First, we need to declare a variable to store our arguments. In Ruby, this is done using the `ARGV` global variable. It's an array that stores all the arguments passed to our script.

```
Ruby
my_arguments = ARGV
```

Next, we can use a loop to iterate through the arguments and output them to the terminal. Let's also add a conditional to check for an empty argument array, just in case our user forgets to pass in any arguments.

```
Ruby
if my_arguments.empty?
  puts "Please provide some arguments."
else
  my_arguments.each do |arg|
    puts "Argument: #{arg}"
  end
end
```

Save your file as `read_args.rb` and let's test it out! Open a terminal and navigate to the directory where you saved your script. We can run our script by typing `ruby read_args.rb` followed by any arguments we want to pass in. For example:

```
Terminal
ruby read_args.rb hello world
```

This should output:
```
Argument: hello
Argument: world
```

## Deep Dive
Now that we have a basic understanding of how to read command line arguments, let's take a deeper look at some of the concepts involved.

One important concept to keep in mind is the distinction between arguments and options. Arguments are the actual values passed to our script after the file name, while options are additional flags or switches that modify the behavior of our script. For example, in the command `ruby script.rb -v`, `-v` is an option.

Additionally, we can access individual arguments using their index in the `ARGV` array. For example, `ARGV[0]` will return the first argument, `ARGV[1]` the second argument, and so on.

It's also worth noting that command line arguments are always passed in as strings, so we may need to perform type conversions if we want to use them as numbers or booleans in our script.

## See Also
Here are some additional resources for reading command line arguments in Ruby:

- [Ruby documentation on ARGV](https://ruby-doc.org/core-3.0.1/ARGF.html)
- [A Beginner's Guide to Command Line Arguments in Ruby](https://www.sitepoint.com/ruby-command-line/)
- [Ruby Command Line Arguments Explained](https://rubyguides.com/ruby-command-line-arguments/)

Happy coding with your newfound knowledge of reading command line arguments in Ruby!