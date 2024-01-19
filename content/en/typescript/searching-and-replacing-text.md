---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Searching and Replacing Text in TypeScript

## What & Why?

Searching and replacing text are basic operations to manipulate strings. Developers pull this trick to find fragments in data or tweak it, typically in scenarios like altering configuration files or handling user inputs.

## How to:

In TypeScript, use the `replace()` method for this. Feed it two values: the string to find (can use a RegExp too), and the string to swap in. Check this out:

```TypeScript
let text = "Hello, World!";
let newText = text.replace("World", "TypeScript");
console.log(newText); // "Hello, TypeScript!"
```

When you run this, "World" in the `text` string gets replaced with "TypeScript", and the `console.log()` call shows the updated string.

To replace all occurrences (not just the first), use a RegExp with a `g` (global) flag, like this:

```TypeScript
let text = "Good morning, good afternoon, good evening";
let newText = text.replace(/good/gi, "great");
console.log(newText); // "Great morning, great afternoon, great evening"
```

Here, `gi` after `/good/` makes the search case-insensitive and replaces all occurrences.

## Deep Dive

Searching and replacing text is old as hills, originating from early command-line tools like `sed` and `awk`. TypeScript's `replace()` method, under the hood, relies on JavaScript's own `replace()`, introduced with the first JavaScript specs in 1997.

An alternative is chaining `split()` and `join()` methods. This method can be more readable to some:

```typescript
let text = "Hello, World! World!";
let newText = text.split('World').join('TypeScript');
console.log(newText); // "Hello, TypeScript! TypeScript!"
```

The `split()` method breaks the string into an array with "World" as the separator. The `join()` method then glues the array back into a string, using "TypeScript" as the new "glue".

However, note that `split()` and `join()` can be slower than `replace()`, especially for larger strings, as they involve additional steps of creating and merging arrays.

## See Also

Here are more sources if you want to dig deeper:

1. MDN's ["String.prototype.replace()"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. A Stack Overflow thread on [Efficiency of string replace vs split-then-join](https://stackoverflow.com/questions/7176441/efficiency-of-string-replace-vs-split-then-join)
3. [TypeScript Docs](https://www.typescriptlang.org/docs/handbook/basic-types.html#string) - to understand more about string manipulation in TypeScript.