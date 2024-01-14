---
title:                "TypeScript recipe: Searching and replacing text"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

When working with a large amount of text, it can be tedious and time-consuming to manually search and replace certain words or phrases. This is where the power of programming comes in, specifically using TypeScript. By utilizing its string manipulation methods, we can easily automate the process of searching and replacing text.

## How To

To start off, we will need a string that we want to perform the search and replace on. For this example, we will use the sentence, "I love TypeScript." We can assign this string to a variable, let's call it `text`.

```TypeScript
let text: string = "I love TypeScript.";
```

To search for a specific word or phrase within the string, we can use the `indexOf()` method. This method will return the index position of the first occurrence of the word or phrase we are searching for. For example, if we want to search for the word "love" within our `text` variable, we can use the following code:

```TypeScript
let index: number = text.indexOf("love");
console.log(index); // Output: 2
```

As we can see, the index position of the word "love" in the string is 2.

Next, we can use the `replace()` method to replace the word "love" with another word or phrase. This method takes in two parameters - the word or phrase we want to replace, and the word or phrase we want to replace it with.

```TypeScript
let newText: string = text.replace("love", "adore");
console.log(newText); // Output: I adore TypeScript.
```

We can also use regular expressions in the `replace()` method to make more complex replacements. For example, if we want to replace all instances of the word "TypeScript" with "JavaScript", we can do so using the following code:

```TypeScript
let newText: string = text.replace(/TypeScript/g, "JavaScript");
console.log(newText); //Output: I love JavaScript.
```

## Deep Dive

The `indexOf()` method can take in a second parameter, which specifies the starting index of the search. This can be useful if we only want to search for a word or phrase after a certain point in the string. For example, if we only want to search for the word "love" after the word "I" in our string, we can use the following code:

```TypeScript
let index: number = text.indexOf("love", 2);
console.log(index); // Output: 2
```

We can also use the `lastIndexOf()` method to search for the last occurrence of a word or phrase in a string. This can be useful if we want to replace the last instance of a word or phrase rather than the first. The syntax for this method is the same as `indexOf()`.

In addition to `replace()`, TypeScript also has a `replaceAll()` method that will replace all instances of a word or phrase in a string. This can be useful for making multiple replacements in one go. The syntax for this method is the same as `replace()`, with the addition of the global `g` flag.

## See Also

- [TypeScript documentation on string methods](https://www.typescriptlang.org/docs/handbook/strings.html#string-methods)
- [Regular expressions in TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)