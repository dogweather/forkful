---
title:    "Ruby recipe: Printing debug output"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Whether you are a beginner or an experienced programmer, you have probably encountered the need to debug your code at some point. Debugging is the process of identifying and resolving errors in your code, and one useful tool for this is printing debug output. Printing debug output allows you to see the values of different variables at different points in your code, giving you a better understanding of how your code is running. In this blog post, we will take a look at how to print debug output in Ruby and why it is a useful technique for debugging.

## How To

To print debug output in Ruby, we can use the `p` or `pp` methods. Let's take a look at an example using the `p` method:

```ruby
age = 30
p age
```

The above code will output `30` to the console, showing us the current value of the `age` variable. We can also use the `pp` method to print a more detailed output, which is useful when debugging objects and nested data types. Here's an example:

```ruby
person = {
  name: "John",
  age: 30,
  hobbies: ["reading", "coding"]
}
pp person
```

The output of this code will be:

```ruby
{:name=>"John", :age=>30, :hobbies=>["reading", "coding"]}
```

This allows us to see the structure and values of an object in a more readable format.

## Deep Dive

Now that we know how to print debug output, let's take a deeper look at why it is a useful debugging technique. One of the main advantages of printing debug output is that it allows us to see the values of different variables at different points in our code. This can help us identify where a problem is occurring and what values the variables have at that specific point. It also allows us to check the flow of our code and make sure that our logic is working as expected.

Another benefit of printing debug output is that it is a non-destructive way of debugging. This means that it does not alter our code in any way, allowing us to easily remove the debugging statements once we have identified and resolved the errors.

One thing to keep in mind when using print debug output is that we should only use it as a temporary tool for debugging. Once we have identified and resolved the errors, we should remove the debugging statements from our code to avoid clutter and improve its readability.

## See Also

- [Ruby Debug Gem](https://rubygems.org/gems/debug)
- [Using "puts" vs "p" vs "print" in Ruby](https://medium.com/rubycademy/different-ways-to-print-debug-output-3ee36e845fb6)
- [Debugging in Ruby: The Ultimate Guide](https://www.rubyguides.com/2019/08/debugging-ruby/)

Happy debugging!