---
title:                "Using regular expressions"
html_title:           "TypeScript recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, commonly known as "regex", are powerful tools used for pattern matching and text manipulation. They are widely used in programming languages, including TypeScript, to help developers search, replace, and validate strings efficiently. Whether you are a beginner or an experienced developer, understanding regular expressions can greatly enhance your code and make certain tasks much easier.

## How To

To use regular expressions in TypeScript, first, we need to declare them as RegEx objects using the `RegExp` constructor. Here's an example:

```TypeScript
let regex: RegExp = /apple/; // This will match any string with the word "apple"
```

To search for a pattern within a string, we can use the `test()` method, which will return a boolean indicating if the pattern is found or not. Here's an example:

```TypeScript
let string: string = "I love apples!";
regex.test(string); // Returns true
string = "I prefer oranges.";
regex.test(string); // Returns false
```

We can also use regular expressions to extract certain parts of a string. For example, if we want to extract a specific word from a sentence, we can use the `exec()` method. Here's an example:

```TypeScript
let string: string = "My favorite fruit is apple.";
let regex: RegExp = /apple/;
let result: RegExpExecArray | null = regex.exec(string);

if (result) {
  console.log(result[0]); // Outputs "apple"
}
```

Regular expressions also have modifiers and flags that can help us modify our pattern matching behavior. For example, the `i` flag can be used to perform case-insensitive matching. Here's an example:

```TypeScript
let regex: RegExp = /apple/i; // This will match "apple", "Apple", "APple", etc.
```

## Deep Dive

Regular expressions can be more complex than simple string matching. They have a set of special characters and metacharacters that can represent a range of characters or patterns. For example, the `.` metacharacter can match any single character, and the `*` can match any number of occurrences of the previous character. Here's an example:

```TypeScript
let regex: RegExp = /a.c*/; // This will match "ac", "abc", "abcc", "acc", etc.
```

We can also use quantifiers to specify how many times a specific character or group should appear. For example, `{n}` matches exactly `n` times, and `{m, n}` matches between `m` and `n` times. Here's an example:

```TypeScript
let regex: RegExp = /\d{4}/; // This will match any four-digit number
```

Regular expressions also have a set of predefined character classes that can help us match certain types of characters, such as digits, letters, or whitespace. We can also use these classes in combination with modifiers and quantifiers. Here's an example:

```TypeScript
let regex: RegExp = /\d{3}\w{3}\s/; // This will match three digits, three letters, and a whitespace character
```

## See Also

- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regexr](https://regexr.com/) - useful tool for testing and learning regular expressions
- [TypeScript Docs - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)