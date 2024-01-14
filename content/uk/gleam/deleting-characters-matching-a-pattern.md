---
title:    "Gleam: Видалення символів, що відповідають шаблону"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Чому

Sometimes when working with large sets of data, we may come across the need to delete characters that match a certain pattern. This could be due to formatting issues or simply to clean up the data. In this blog post, we will explore how to do this using the Gleam programming language.

## Як це зробити

The first step is to import the `Strings` module, which contains functions for manipulating strings in Gleam. Then, we can use the `delete_pattern` function to delete characters that match a given pattern. Let's take a look at an example:

```Gleam
import gleam/strings

let string = "Gleam is a fun programming language!"

let new_string = strings.delete_pattern(string, "[a-z]")

debug!($new_string)
```

In this code, we first create a string variable containing a sentence. Then, we use the `delete_pattern` function to delete all lowercase letters from the string. Finally, we use the `debug!` macro to print the result. The output should be: "G.!". As we can see, all lowercase letters have been successfully deleted from our string.

## Глибокий занурення

The `delete_pattern` function is just one of many useful functions for string manipulation in Gleam. It utilizes regular expressions to match patterns, giving us a lot of flexibility in what we can delete from our strings. Additionally, we can use the `delete` function to delete specific characters, or the `strip` function to remove leading and trailing whitespace from a string. These functions are essential tools for data cleaning and formatting in Gleam.

## Дивись також

- [Gleam документація по модулю Strings](https://gleam.run/documentation/stdlib/strings)
- [Регулярні вирази в Gleam](https://gleam.run/documentation/syntax/regular-expressions)
- [Приклади коду Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)