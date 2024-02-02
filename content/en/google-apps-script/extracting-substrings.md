---
title:                "Extracting substrings"
date:                  2024-02-01T13:42:04.708963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Extracting substrings"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means getting a portion of text from a larger string, basically cutting it down to size. Programmers do this to pinpoint specific data within strings, like pulling an ID from a URL or a tag from an HTML element.

## How to:

Google Apps Script, being based on JavaScript, offers a straightforward way to handle substrings. Let’s dive into examples using `substring()`, `slice()`, and `substr()` methods. 

First up, `substring(startIndex, endIndex)`: 

```Javascript
var fullString = "Hello, Google Apps Script!";
var partOfString = fullString.substring(7, 11);
Logger.log(partOfString); // Outputs: Google
```

Next, `slice(startIndex, endIndex)` – similar to `substring` but can accept negative indices:

```Javascript
var fullString = "Hello, Google Apps Script!";
var partOfString = fullString.slice(-7, -1);
Logger.log(partOfString); // Outputs: Script
```

Finally, `substr(startIndex, length)` – note this uses length from the start index, not an end index:

```Javascript
var fullString = "Hello, Google Apps Script!";
var partOfString = fullString.substr(7, 6);
Logger.log(partOfString); // Outputs: Google
```

## Deep Dive

The methods `substring()`, `slice()`, and `substr()` have been around since early JavaScript days, but their usage in Google Apps Script brings modern convenience to manipulating strings within the extended Google Apps ecosystem. While `substring()` and `slice()` are more commonly used and recommended due to their clarity (they work with start and end indices), `substr()` might be handy in situations where you're thinking in terms of "start point and length." However, it's worth noting that `substr()` has been considered a legacy function and may be deprecated in future ECMAScript versions; thus, using `substring()` or `slice()` is generally safer and future-proof for your scripts. Remember, the choice between `slice()` and `substring()` often boils down to preference, unless you need to use negative indices—then `slice()` is your go-to. With Google Apps Script being updated regularly, leveraging these methods allows for efficient and effective string manipulation, pivotal for processing text in automation, custom functions, or scripts interfacing with Google Sheets, Docs, and other Google services.
