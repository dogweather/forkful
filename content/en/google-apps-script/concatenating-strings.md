---
title:                "Concatenating strings"
date:                  2024-02-01T13:42:02.798289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Concatenating strings"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is just a fancy term for gluing text together. Programmers do it to combine data and messages in a way that's meaningful for users and other parts of the code. Pretty basic, but crucial.

## How to:

In Google Apps Script, which is based on JavaScript, you've got a couple of straightforward ways to concatenate, or combine, strings. Let's explore them with some examples.

### Using the `+` operator:

It's the most common method. You just use the plus symbol between your strings. Check it:

```Javascript
function concatenatePlus() {
  var firstName = "Jane";
  var lastName = "Doe";
  var fullName = firstName + " " + lastName;
  Logger.log(fullName); // Outputs: Jane Doe
}
```

### Using template literals:

This is a cleaner way introduced in ES6, making your code easier to read and write, especially with variables.

```Javascript
function concatenateTemplateLiteral() {
  var firstName = "Jane";
  var lastName = "Doe";
  var fullName = `${firstName} ${lastName}`;
  Logger.log(fullName); // Outputs: Jane Doe
}
```

### Using Array's `join()` method:

Not the first method you might think of, but it's useful when you have a bunch of strings.

```Javascript
function concatenateJoin() {
  var names = ["Jane", "Doe"];
  var fullName = names.join(" ");
  Logger.log(fullName); // Outputs: Jane Doe
}
```

## Deep Dive

Before the advent of template literals in ECMAScript 2015 (ES6), concatenating strings, especially with variables and across multiple lines, was a bit cumbersome, involving lots of `+` operators or array `join()` shenanigans. Template literals provided a more readable and concise syntax, which Google Apps Script, being based on JavaScript, benefits from.

However, while template literals are cleaner and easier to use, especially with embedded expressions, the traditional `+` operator or `join()` might still be your go-to in some scenarios. For instance, if you're dealing with a large array of strings that needs to be combined into a single string, the `join()` method can be more performant.

Remember, the choice of method depends largely on what makes your code more readable and maintainable for you and your team. There's no one-size-fits-all answer, but in most cases, template literals can offer a more modern and elegant solution.
