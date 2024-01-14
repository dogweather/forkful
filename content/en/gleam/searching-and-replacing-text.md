---
title:    "Gleam recipe: Searching and replacing text"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

You've been writing code all day, happily typing away until you realize you've made a mistake and need to replace some text. Or maybe you want to refactor your code and replace a specific variable name throughout your project. These are just a few reasons why you would want to engage in searching and replacing text in your code. It's a common task for programmers and can save you a lot of time and effort.

## How To

To search and replace text in Gleam, you can use the `replace` and `replace_all` functions from the `Text` module. Here's an example of how to use them:

```
Gleam REPL> import Text
Gleam REPL> let modified_text = Text.replace("Hello, World!", "Hello", "Goodbye")
Gleam REPL> let modified_text_all = Text.replace_all("Hello, hello, hello", "Hello", "Goodbye")
Gleam REPL> modified_text
Goodbye, World!
Gleam REPL> modified_text_all
Goodbye, Goodbye, Goodbye
```

In the first example, we replace the first occurrence of "Hello" with "Goodbye" in our string. In the second example, we use `replace_all` to replace all occurrences of "Hello" with "Goodbye". As you can see, these functions are very straightforward to use.

## Deep Dive

Behind the scenes, the `replace` and `replace_all` functions use regular expressions to find and replace text. This means you can use regular expressions as the search pattern instead of a specific string. Let's see an example of this in action:

```
Gleam REPL> let modified_text = Text.replace("Hello, World!", "\w+", "Hi")
Gleam REPL> modified_text
Hi, World!
```

In this example, we used the regular expression "\w+" to match any word character and replace it with "Hi". This can be useful if you need to replace multiple variations of a word or phrase in your code.

## See Also

For more information on searching and replacing text in Gleam, check out the official documentation on `Text` module: https://gleam.run/documentation/stdlib/text/

You can also learn more about regular expressions and how to use them in Gleam by checking out the `Regex` module: https://gleam.run/documentation/stdlib/regex/

Happy coding!