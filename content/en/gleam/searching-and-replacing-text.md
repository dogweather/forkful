---
title:    "Gleam recipe: Searching and replacing text"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in a situation where you need to make a simple change to a large piece of text? Maybe you want to replace a word or phrase with something else, or maybe you need to fix a typo in multiple places. Regardless of the reason, manually going through and making these changes can be time-consuming and prone to errors. That's where searching and replacing text comes in handy. With the help of Gleam, you can easily automate this task and save yourself time and effort.

## How To

To search and replace text in Gleam, we will be using the `String.replace` function. This function takes in three arguments: the string to be searched, the text to be replaced, and the replacement text. Let's take a look at a simple example:

```
Gleam - 1.0
import gleam/string

pub fn main() {
  let text = "Hello, world!";
  let new_text = text |> string.replace("world", "Gleam");
  
  assert.equal(new_text, "Hello, Gleam!");
}
```

In this example, we define a string variable `text` and then use the `|>` operator to pipe that string into the `replace` function. This function then replaces the word "world" with "Gleam" and assigns the result to the variable `new_text`. Finally, we use an `assert` statement to confirm that the result is what we expected.

But what if we want to replace multiple instances of a word or phrase? Gleam's `replace` function also accepts a fourth argument, `count`, which allows us to specify the number of occurrences to replace. For example:

```
Gleam - 1.0
import gleam/string

pub fn main() {
  let text = "I love apples, apples are my favorite fruit!";
  let new_text = text |> string.replace("apples", "oranges", 1);
  
  assert.equal(new_text, "I love oranges, apples are my favorite fruit!");
}
```

This time, we have specified a `count` of 1, so only the first instance of "apples" is replaced with "oranges". The rest of the string remains unchanged.

## Deep Dive

Under the hood, the `replace` function uses the `Regex`, or regular expressions, module to search for and replace text. This means you can also use regex patterns in your search and replacement strings. For example, if we wanted to replace all instances of a number followed by a period with the word "item", we could do it like this:

```
Gleam - 1.0
import gleam/regex
import gleam/string

pub fn main() {
  let text = "I bought 3 apples today";
  let new_text = text |> regex.replace_all("(\d+)\.", "item");
  
  assert.equal(new_text, "I bought item apples today");
}
```

The `\d` pattern matches any digit, and the `+` means "one or more times". So in this case, we are replacing any number of digits followed by a period with the word "item".

## See Also

- Gleam Documentation: <https://gleam.run/>
- Regular Expressions in Gleam: <https://gleam.run/docs/regexes>
- String Module in Gleam: <https://gleam.run/docs/strings>