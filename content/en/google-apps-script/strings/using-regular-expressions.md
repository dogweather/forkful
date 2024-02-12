---
title:                "Using regular expressions"
aliases:
- /en/google-apps-script/using-regular-expressions.md
date:                  2024-02-01T21:12:17.926556-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using regular expressions"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular Expressions (regex) are patterns used to match character combinations in strings. Programmers utilize them for searching, editing, or manipulating text and data, making them indispensable for pattern matching and data parsing tasks.

## How to:

Using regular expressions in Google Apps Script is straightforward thanks to the JavaScript-based syntax. Here's how you can incorporate regex into your scripts for common tasks like searching and data validation.

### Searching Strings

Suppose you want to find if a string contains a specific pattern, such as an email address. Here’s a simple example:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Found: " + found[0]);
  } else {
    Logger.log("No email found.");
  }
}

// Sample usage
findEmailInText("Contact us at info@example.com.");
```

### Data Validation

Regular expressions shine in data validation. Below is a function that validates an input string to check if it adheres to a simple password policy (at least one uppercase letter, one lowercase letter, and a minimum of 8 characters).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Sample output
Logger.log(validatePassword("Str0ngPass")); // Outputs: true
Logger.log(validatePassword("weak"));       // Outputs: false
```

## Deep Dive

Regular expressions in Google Apps Script are inherited from JavaScript, first standardized in ECMAScript language specification in June 1997. Although powerful, they can sometimes lead to confusing and hard-to-maintain code, especially when overused or used for complex pattern matching tasks that might be more efficiently solved through other parsing methods.

For instance, while you can use regex for HTML or XML parsing in a pinch, doing so is generally discouraged due to the nested and intricate structures of these documents. Instead, tools specifically designed for parsing such structures, like DOM parsers for HTML, are more reliable and readable.

Moreover, Google Apps Script developers should be mindful of potential performance issues when using complex regex patterns in large-scale text manipulation tasks, as regex processing can be CPU-intensive. In such cases, breaking the task into simpler sub-tasks or using built-in string manipulation functions could offer a better balance of performance and maintainability.
