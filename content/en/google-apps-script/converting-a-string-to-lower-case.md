---
title:                "Converting a string to lower case"
date:                  2024-02-01T13:42:02.964186-07:00
model:                 gpt-4-0125-preview
simple_title:         "Converting a string to lower case"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
`Converting a string to lower case` is about transforming all the characters in a given string to their lowercase form. Programmers do this for consistency, especially in cases where the case of the characters could lead to errors or misinterpretation of data, such as user input validation or implementing case-insensitive search.

## How to:
Google Apps Script makes converting strings to lowercase straightforward with the `.toLowerCase()` method. This method works directly on the string object. Here’s how you do it:

```Javascript
function convertToLowercase() {
  var originalString = "Hello, World!";
  var lowercaseString = originalString.toLowerCase();
  
  Logger.log(lowercaseString); // Outputs: hello, world!
}
```

This simple function illustrates converting the string `"Hello, World!"` to all lowercase letters. The `Logger.log()` method is used to output the result in the Apps Script's log. You can run this script in any Google Apps Script environment, such as a Script Editor within Google Sheets or Google Docs.

## Deep Dive
The `.toLowerCase()` method in JavaScript, and by extension Google Apps Script (since GAS is based on JavaScript), doesn't discriminate between languages – it will convert all applicable characters within the string to their lowercase equivalents based on the Unicode standard. However, there's a catch; not all scripts (like some Asian scripts) have the concept of letter cases as Latin scripts do. In such contexts, `.toLowerCase()` simply returns the original string unaffected.

It's also worth noting that while `.toLowerCase()` is a robust method for case conversion in most applications, certain locale-specific case mappings can't be handled by it. For example, the Turkish dotless 'ı' and dotted 'I' transformations don't follow the generic Unicode case mapping rules. For such specific needs, JavaScript introduced `.toLocaleLowerCase()`, which Google Apps Script supports as well. This method can take locale as an argument and perform the case conversion based on locale-specific rules, which might be a better alternative in applications serving a global audience.

In practice, `.toLowerCase()` covers the requirements for most simple applications and remains the go-to method for converting a string to lowercase in Google Apps Script. However, for developers working on more locale-sensitive applications, exploring `.toLocaleLowerCase()` would be wise.
