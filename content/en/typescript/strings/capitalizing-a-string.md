---
date: 2024-02-03 19:02:40.500186-07:00
description: "Capitalizing a string involves modifying the first character of a given\
  \ string to uppercase if it is in lowercase, often leaving the rest of the string\u2026"
lastmod: '2024-03-13T22:44:59.842965-06:00'
model: gpt-4-0125-preview
summary: "Capitalizing a string involves modifying the first character of a given\
  \ string to uppercase if it is in lowercase, often leaving the rest of the string\u2026"
title: Capitalizing a string
weight: 2
---

## What & Why?
Capitalizing a string involves modifying the first character of a given string to uppercase if it is in lowercase, often leaving the rest of the string unchanged. This operation is typically used to ensure proper nouns or the beginnings of sentences adhere to grammatical rules in text processing, making outputs appear professional and readable.

## How to:

TypeScript, being a superset of JavaScript, allows for various methods to capitalize strings, ranging from pure JavaScript approaches to utilizing third-party libraries for more complex or specific use cases.

**Pure JavaScript Approach:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Sample Output:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

This method is straightforward and relies on the `charAt()` method to access the first character of the string and `toUpperCase()` to convert it to uppercase. The `slice(1)` method then retrieves the rest of the string, leaving it unchanged.

**Using Lodash Library:**

For projects already using the [Lodash](https://lodash.com/) library, you can utilize its `_.capitalize` function to achieve the same result with less boilerplate code.

First, install Lodash:

```bash
npm install lodash
```

Then, use it in your TypeScript file:

```typescript
import * as _ from 'lodash';

// Sample Output:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Note: Lodash's `_.capitalize` method converts the rest of the string to lowercase which might not always be what you want.

**Using a Regular Expression:**

A regular expression can provide a concise way to capitalize the first letter of a string, especially if you need to capitalize the first letter of each word in a string.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Sample Output:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

This method uses the `replace()` function to search for any word boundary followed by an alphanumeric character (`\b\w`), capitalizing each match. It's particularly handy for titles or headings.
