---
title:                "Refactoring"
date:                  2024-02-01T13:42:04.896870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/refactoring.md"
---

{{< edit_this_page >}}

## What & Why? 
Refactoring is the programming practice of cleaning up your code without changing what it does. Programmers do it to make code easier to understand and cheaper to modify.

## How to:
Let's dive into an example. Imagine you've written a function to log when a document in Google Drive was last edited. Initially, your code might look something like this:

```Google Apps Script
function logLastEdited() {
  var file = DriveApp.getFileById('your-file-id-here');
  var lastUpdated = file.getLastUpdated();
  Logger.log('File was last updated on: ' + lastUpdated);
}
```

This does the job but mixes finding the file and logging the date. Let’s refactor it by splitting into two functions for clarity:

```Google Apps Script
function logLastEdited() {
  var lastUpdated = getLastUpdated('your-file-id-here');
  Logger.log('File was last updated on: ' + lastUpdated);
}

function getLastUpdated(fileId) {
  var file = DriveApp.getFileById(fileId);
  return file.getLastUpdated();
}
```

Now, `logLastEdited` is more focused, and `getLastUpdated` can be reused elsewhere. This is a simple form of refactoring, focusing on making each function do one thing well.

## Deep Dive
The concept of refactoring isn't new and transcends programming languages, coming into broader awareness with the publication of Martin Fowler’s book "Refactoring: Improving the Design of Existing Code" in the late '90s. In Google Apps Script, due to its nature of being closely tied with Google Services like Drive, Sheets, etc., refactoring can often mean breaking down large functions that interact with these services into smaller, more manageable pieces. This not only makes the codebase more understandable and maintainable but also can lead to performance improvements by optimizing API call usage.

While Google Apps Script itself doesn’t have built-in refactoring tools like some IDEs (Integrated Development Environments) do, understanding the principles of refactoring and applying them diligently can greatly improve the quality of your scripts. Tools like clasp can help manage and develop Apps Script projects in local development environments, where more powerful code editors can assist in the refactoring process.

However, because of its JavaScript roots, many principles and patterns from JavaScript refactoring apply directly to Google Apps Script. You can leverage JavaScript's rich ecosystem of linters and formatters (e.g., ESLint, Prettier) to enforce consistent coding standards and spot potential refactoring opportunities.

Remember, the key to effective refactoring in Google Apps Script, or any programming language for that matter, lies in small, incremental improvements and keeping the code's functionality unchanged while enhancing its internal structure.
