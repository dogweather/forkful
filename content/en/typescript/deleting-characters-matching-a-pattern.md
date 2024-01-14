---
title:                "TypeScript recipe: Deleting characters matching a pattern"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

As developers, we often come across situations where we need to manipulate strings in our code. One common task is to delete characters that match a specific pattern within a string. This can be useful when filtering out unwanted characters or formatting data in a specific way. In this post, we will explore how to efficiently delete matching characters in TypeScript.

## How To

To delete characters matching a pattern in TypeScript, we can use regular expressions or methods provided by the String class. Let's take a look at some examples using both approaches.

### Using Regular Expressions

Regular expressions are a powerful tool for matching patterns in strings. In TypeScript, we can create a regular expression object using the `RegExp` class. The `test()` method can then be used to check if a string matches the pattern.

```typescript
const regex = new RegExp("[aeiou]", "gi");
const str = "Hello World";
const result = str.replace(regex, "");
console.log(result); // Hll Wrld
```

In this example, we create a regular expression that matches all vowels globally and case-insensitively. We then use the `replace()` method to replace all matching characters with an empty string, effectively deleting them from the original string.

### Using String Methods

The String class in TypeScript provides several methods to manipulate strings, including `replace()` and `split()`. We can use these methods to delete characters matching a pattern in a similar way.

```typescript
const str = "Hello World";
const result = str.replace(/[aeiou]/gi, "");
console.log(result); // Hll Wrld
```

In this example, we use the `replace()` method and pass in a regular expression as the first argument. This approach is more concise and can be useful for simpler patterns.

## Deep Dive

Now that we've seen how to delete matching characters using regular expressions and methods provided by the String class, let's take a closer look at regular expressions and their syntax.

Regular expressions are patterns used to match character combinations in strings. They are delimited by forward slashes (`/`) and can include flags to modify the matching behavior. In the first example, we used the `gi` flags, where `g` stands for global and `i` stands for case-insensitive.

The `i` flag makes the pattern case-insensitive, so it will match both upper and lower-case letters. The `g` flag makes the regular expression search for all matching occurrences in the string instead of just the first one.

## See Also

- [TypeScript Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [String Methods in TypeScript](https://www.geeksforgeeks.org/typescript-string-methods/)
- [RegExp Class in TypeScript](https://www.yelp.com/developer/documentation/v3/get_started#regular-expressions)