---
title:                "Searching and replacing text"
date:                  2024-02-01T13:42:22.301451-07:00
model:                 gpt-4-0125-preview
simple_title:         "Searching and replacing text"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text in Google Apps Script is about finding specific strings within a document (or any text-based file) and swapping them out for something else. Why bother? Well, it's perfect for automating edits across large documents or datasets, saving time, and avoiding human error.

## How to:

Let's dive straight into making these changes with some straightforward code. Suppose you've got a Google Doc and you want to replace every instance of "oldText" with "newText". Here's how you'd do it:

```Javascript
function replaceTextInDocs() {
  var doc = DocumentApp.getActiveDocument(); // Gets the active Google Doc
  var body = doc.getBody(); // Accesses the body of the doc
  
  body.replaceText('oldText', 'newText'); // Replaces oldText with newText
}
```

This simple script grabs the active document you're working on, accesses its body, and performs a search/replace operation. But what if you're dealing with a Google Sheet and want to perform a similar operation? Here’s an example:

```Javascript
function replaceTextInSheets() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet(); // Gets the active sheet
  var range = sheet.getDataRange(); // Gets the range containing data
  var values = range.getValues(); // Retrieves all data in range as a 2D array
  
  // Iterate over each row and column, replacing text
  for (var i = 0; i < values.length; i++) {
    for (var j = 0; j < values[i].length; j++) {
      if (typeof values[i][j] == 'string') { // Checks if the cell contains text
        values[i][j] = values[i][j].replace(/oldText/g, 'newText'); // Replaces oldText with newText
      }
    }
  }
  
  range.setValues(values); // Writes the modified data back to the sheet
}
```

This snippet tackles a Google Sheets scenario, reading through each cell, checking if it's a string, and performing the replacement. Notice how we use a regular expression with the global flag `/g` to ensure all occurrences are replaced, not just the first one.

## Deep Dive

The ability to search and replace text programmatically in Google Apps Script is based on standard string manipulation techniques present in many programming languages, tailored for Google's environment. Introduced as part of Google's suite of automation tools, this functionality taps into the vast ecosystem of Google Apps, allowing scripts to interact seamlessly with documents, spreadsheets, presentations, and more.

Historically, manual text replacement was a tedious, error-prone process. The development of scripting languages, particularly those integrated with web and cloud services like Google Apps Script, revolutionized this task. However, while Google Apps Script provides a convenient, directly integrated way to automate text replacements in Google Docs, Sheets, and other G Suite applications, it's not without limitations. Performance can lag with very large datasets or documents, and complex regular expression capabilities are somewhat limited compared to more powerful programming languages like Python.

For heavy-duty text processing or more intricate pattern matching requirements, you might consider writing a standalone script in a more powerful language and using the Google Drive API to fetch and update files. Nevertheless, for quick edits, simple document processing, and those already working within the Google ecosystem, Google Apps Script offers a straightforward, accessible solution.
