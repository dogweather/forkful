---
title:                "Parsing a date from a string"
aliases:
- en/typescript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:02:41.219107-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing a date from a string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string involves converting textual representations of dates and times into a format that can be manipulated and analyzed by the program. This is a common task in programming as it allows for the handling of user input, storage of time-stamped data, and interactions with APIs, yielding more functional and user-friendly applications.

## How to:
TypeScript, being a superset of JavaScript, relies on the Date object for parsing dates from strings. However, working with dates in JS/TS can become verbose or imprecise due to the quirks of the Date object. Here's a basic example followed by an approach using a popular library, `date-fns`, for more robust solutions.

### Using JavaScript's Date Object
```typescript
// Basic parsing using the Date constructor
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Output for GMT: "Fri Apr 21 2023 15:00:00 GMT+0000 (Coordinated Universal Time)"
```

This method works for ISO format strings and some other date formats, but may yield inconsistent results for ambiguous formats across browsers and locales.

### Using date-fns
The `date-fns` library provides straightforward and consistent handling of dates. It's a modular library, allowing you to include only the parts you need, reducing bundle size.

First, install `date-fns`: 

```sh
npm install date-fns
```

Then, use it to parse a date string:

```typescript
import { parseISO, format } from 'date-fns';

// Parsing an ISO string
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Formatting the date (e.g., into a human-readable form)
console.log(format(parsedDate, "PPPpp")); 
// Output: "Apr 21st, 2023 at 3:00 PM" (output may vary based on locale)
```

`date-fns` supports a wide variety of formats and locales, making it a robust choice for applications that necessitate precise date parsing and formatting across different user regions.
