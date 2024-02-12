---
title:                "Extracting substrings"
aliases: - /en/google-apps-script/extracting-substrings.md
date:                  2024-02-01T21:11:56.433402-07:00
model:                 gpt-4-0125-preview
simple_title:         "Extracting substrings"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings involves taking a portion of a string - essentially creating a new string from part of an existing one. Programmers do this for a myriad of reasons, including data parsing, text manipulation for user interfaces, or processing input for various applications, making substring extraction a versatile tool in any scripting arsenal.

## How to:

In Google Apps Script, which is based on modern JavaScript, substring extraction can be achieved through several methods, including `substring()`, `substr()`, and `slice()`. Each has its nuances, but they all serve the purpose of pulling specified characters from a string.

```javascript
// Example using substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Output: Hello

// Example using substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Output: world

// Example using slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Output: world!
```

Each method takes two arguments: the start position and, except for `slice()` which can accept negative indices to start from the end, the end position or the number of characters to extract. It's worth noting that the original string remains unchanged after these operations, as they return new string values.

## Deep Dive

Historically, the JavaScript methods for extracting substrings have been a source of confusion due to their similar names and functionalities. However, in Google Apps Script and modern JavaScript, `substring()` and `slice()` are most frequently used, with `substr()` being considered deprecated. This is important to note for those writing future-proof code.

The main difference between `substring()` and `slice()` is how they handle negative indexes; `substring()` treats negative indices as 0, while `slice()` can accept a negative index to start the extraction from the end of the string. This makes `slice()` particularly handy for cases where the exact length of the string might not be known or when needing to extract from the end.

When deciding which method to use for substring extraction, the choice often boils down to the specific requirements of the operation (e.g., whether handling negative indices is beneficial) and personal or team coding standards. While there's no one-size-fits-all best practice, understanding the subtle differences and performance implications can help make an informed decision.
