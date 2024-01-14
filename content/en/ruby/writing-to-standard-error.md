---
title:                "Ruby recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, one of the common challenges developers face is debugging and troubleshooting their code. In order to identify and fix any errors, it is important to have proper error handling in place. One useful tool for this is writing to standard error, also known as stderr. This allows developers to receive detailed error messages when running their code, making it easier to pinpoint and resolve any issues.

## How To

Writing to standard error in Ruby is simple and can be done using the `STDERR` constant. Let's take a look at an example:

```Ruby
def divide_numbers(num1, num2)
  if num2 == 0
    STDERR.puts "Error: Cannot divide by zero."
  end
  return num1 / num2
end

puts divide_numbers(10, 0)
```

In the above code snippet, we have a method that divides two numbers. However, if the second number is 0, we want to print an error message to standard error. This is done using the `STDERR.puts` method. If we were to run this code, we would see the following output:

```
Error: Cannot divide by zero.
```

We can also use `STDERR.puts` to output other types of errors, such as exceptions and warnings. This allows us to have more control over the error messages we receive when running our code.

## Deep Dive

Now that we know how to write to standard error, let's dive a bit deeper into what it is and why it is useful. In Ruby, standard error is a predefined output stream that is used to display error messages. It is often used alongside standard output, which is the default output stream when using `puts` or `print` statements.

One of the benefits of writing to standard error is that it helps to keep our code organized and easier to read. By separating error messages from regular program output, we can quickly identify any issues that may arise while running our code.

Another advantage of using standard error is that it allows us to redirect error messages to a separate file. This can be useful when running large batch jobs or scripts, as we can easily review any errors that may have occurred without having them clutter up our main output.

## See Also

- [Ruby - Standard Error](https://ruby-doc.org/core-3.0.2/StandardError.html)
- [Difference between standard output and standard error](https://stackoverflow.com/questions/17602886/what-is-the-purpose-of-stderr)
- [Debugging with Standard Error in Ruby](https://blog.newrelic.com/engineering/debugging-with-standard-error-ruby/)

Writing to standard error is a useful technique to have in your programming arsenal. Not only does it make debugging easier, but it also helps to keep your code organized and efficient. Give it a try in your next coding project and see the benefits for yourself!