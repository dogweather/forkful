---
title:                "Reading command line arguments"
date:                  2024-02-01T13:42:15.995217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reading command line arguments"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

So, you wanna grab command line args in Google Apps Script, huh? Well, it's all about fetching parameters when a script is run from a terminal/command line. Why? Mostly because it lets your script behave differently depending on those sweet, sweet inputs.

## How to:

Heads up, Google Apps Script (GAS) doesn't roll with the traditional command-line arguments you'd expect in, say, Python or Node.js. But, don't sweat it; we can mimic the vibe with Web Apps or Google Sheets custom functions that take input parameters. Let's dive into the Google Sheets way because it's pretty hands-on.

First, picture you've got a function that needs some user-provided numbers to do its magic:

```Google Apps Script
function addNumbers(number1, number2) {
  return number1 + number2;
}
```

Nothing fancy, just adding numbers. Now, when you wanna use this in your Sheets, you'd typically call it like `=addNumbers(2, 3)` in a cell, and bam, you got 5.

But what about simulating command line args? You can use Google Sheets custom menus. Check this out:

```Google Apps Script
function onOpen() {
  var ui = SpreadsheetApp.getUi();
  // Adds a custom menu to your sheet
  ui.createMenu('Custom Commands')
  .addItem('Add Numbers', 'showPrompt')
  .addToUi();
}

function showPrompt() {
  var ui = SpreadsheetApp.getUi();
  var result = ui.prompt(
    'Let\'s add some numbers!',
    'Enter two numbers separated by a comma:',
    ui.ButtonSet.OK_CANCEL);

  var button = result.getSelectedButton();
  var text = result.getResponseText();
  
  if (button == ui.Button.OK) {
    var numbers = text.split(',').map(function(str) { return Number(str.trim()); });
    ui.alert('The result is: ' + addNumbers(numbers[0], numbers[1]));
  }
}
```

This snazzy piece of code adds a custom menu to your Google Sheet. Clicking "Add Numbers" prompts you to input some numbers which it then feeds to our `addNumbers` function. Feels like command line args, right?

## Deep Dive

GAS's environment is unique; it's cloud-based and tied to Google's ecosystem (Sheets, Docs, etc.), making traditional command-line arg reading a no-show. Historically, GAS was designed for extending Google Apps functionality, not serving as a standalone backend where you might expect command-line interaction.

Given this, innovative approaches like Web Apps (for HTTP GET/POST params) or using Google Sheets' UI elements (as shown) become alternatives. While these aren't command line args in the purest sense, they do provide a way to dynamically input data into GAS functions.

For hardcore command-line or terminal-based interactions with scripts, you might want to look at Google Cloud Functions or Apps Script API executions via cURL or Postman, which allow for more conventional command-line argument handling through HTTP requests. These alternatives offer deeper integration and broader flexibility for developers exiting the Google Apps Script sandbox, seeking more control and functionality.
