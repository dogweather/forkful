---
date: 2024-02-03 19:02:56.534396-07:00
description: 'How to: To start, you can create a simple regex pattern and use it to
  find matches in a string. Here, we''ll find the word "code".'
lastmod: '2024-03-13T22:45:00.424298-06:00'
model: gpt-4-0125-preview
summary: To start, you can create a simple regex pattern and use it to find matches
  in a string.
title: Using regular expressions
weight: 11
---

## How to:


### Basic Matching
To start, you can create a simple regex pattern and use it to find matches in a string. Here, we'll find the word "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Using `String.prototype.match()`
To retrieve an array of matches:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Global Search
To find all the matches, use the `g` flag:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Case-insensitive Matching
The `i` flag ignores case:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Replacing Text
Use `String.prototype.replace()` to replace parts of the string:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Using Groups
Groups can capture parts of the pattern:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Third-Party Libraries
Although JavaScript's built-in regex capabilities are powerful, some tasks might be simplified with libraries like `XRegExp`. It offers additional syntax and flags, making complex patterns more readable:

```javascript
// XRegExp library example
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

This snippet demonstrates using `XRegExp` to match all Unicode words in a string, showcasing the library’s ability to handle extended character sets beyond JavaScript’s built-in capabilities.
