---
title:                "Finding the length of a string"
date:                  2024-02-01T13:42:02.788486-07:00
model:                 gpt-4-0125-preview
simple_title:         "Finding the length of a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

So, you're tinkering with Google Apps Script and stumbled upon a scenario where you've gotta figure out how long a string is. Whether it's validating input lengths or just some curiosity-driven operation, knowing the string's length is pretty fundamental and can be handy in myriad situations.

## How to:

In Google Apps Script, which is essentially JavaScript for G Suite, finding a string's length is a walk in the park. You'd use the `.length` property. Yeah, it's that straightforward. Here's how it works:

```javascript
function getStringLength() {
  var exampleString = 'Hello, world!';
  var lengthOfString = exampleString.length;
  Logger.log(lengthOfString);
  // Output: 13
}

function anotherExample() {
  var emptyString = ''; // Let's try with an empty string this time
  Logger.log(emptyString.length);
  // Output: 0
}

function countingNewLinesAndSpaces() {
  var stringWithSpaces = '  Hello\nworld  ';
  Logger.log(stringWithSpaces.length);
  // Output: 15, Yep, spaces and new lines count too!
}
```

In these snippets, we're simply declaring strings and using `.length` to get their sizes. The logger then spits out the lengths, showing how many characters are in each string.

## Deep Dive

The concept of string length is straightforward in most programming languages, and Google Apps Script is no exception. The `.length` property is an inherited feature from JavaScript, providing the count of characters in a string, including spaces and special characters. 

Historically, this approach to counting the length of a string has been the standard in JavaScript since its inception. There hasn't been a significant shift or a better alternative for this particular action due to its simplicity and efficiency.

However, it's essential to keep in mind that the `.length` property counts all characters, including trailing spaces, new lines, and other non-visible characters. This might not be what you want in certain situations, like when you're trying to validate user input, and you might need to trim the string or use regular expressions to only count specific characters.

While Google Apps Script doesn't offer anything out of the box that's drastically different or better for getting string lengths compared to vanilla JavaScript, the `.length` property remains a reliable, efficient way to achieve this task. As you dive deeper into Google Apps Script, you'll find this simple operation foundational in various scripting scenarios involving text manipulation.
