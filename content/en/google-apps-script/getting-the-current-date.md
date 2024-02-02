---
title:                "Getting the current date"
date:                  2024-02-01T13:43:34.029170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Getting the current date"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

So you wanna grab the current date in Google Apps Script, huh? It’s all about snagging the exact day we're in right now, which is super handy for anything from timestamping data entries to setting deadlines in your apps. It's like the programming equivalent of checking your watch.

## How to:

Getting the current date in Google Apps Script is like taking candy from a baby, thanks to the built-in `Date` object. Check it:

```Javascript
function getCurrentDate() {
  var today = new Date();
  Logger.log(today);
}
```

When you run `getCurrentDate()`, the Logger's gonna show you something like this (depending on when you check it, obviously):

```
Fri Apr 14 2023 00:00:00 GMT+0000 (Coordinated Universal Time)
```

Want it in a more human-friendly format? No problemo. Let's format it:

```Javascript
function getFormattedCurrentDate() {
  var today = new Date();
  var formattedDate = Utilities.formatDate(today, Session.getScriptTimeZone(), "yyyy-MM-dd");
  Logger.log(formattedDate);
}
```

Now, when you hit run on `getFormattedCurrentDate()`, you'll see:

```
2023-04-14
```

Slick, right? 

## Deep Dive

Now, sinking our teeth a bit deeper, the `Date` object comes from JavaScript, which is the backbone of Google Apps Script. So, it’s not specifically a GAS feature but rather a borrowing from its parent language. This is cool because it means if you've got the hang of JavaScript's `Date`, you're all set here.

Historically, dealing with dates and times in any programming language can be a headache - time zones, leap years, daylight saving changes, oh my! Google Apps Script tries to keep it simple by leaning on JavaScript’s capabilities, but sometimes you might find it a bit limiting, especially for complex date manipulations or heavy-duty time zone juggling.

In such cases, you might consider using libraries (like the popular Moment.js, although it's now considered a legacy project in favor of more modern libraries like Luxon or date-fns) that can be included in your Apps Script projects for more complex operations. Though remember, every external library adds a bit more heft to your project, so use them judiciously.

For most day-to-day needs, though, the native `Date` object, coupled with Google Apps Script’s own formatting functions, should have you covered for getting the current date and making it look pretty for any purpose you've got in mind.
