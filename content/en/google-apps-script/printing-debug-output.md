---
title:                "Printing debug output"
date:                  2024-02-01T13:42:06.470736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Printing debug output"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output in Google Apps Script is all about tracking the flow of your program and inspecting variable values at runtime. Programmers do this to understand where things go awry or to ensure their logic is being followed as expected.

## How to:

In Google Apps Script, the most straightforward way to print debug output is using the `Logger` class for simple logs or `console.log()` for more detailed information, including stack traces in certain contexts, such as in bound scripts to Google Sheets, Docs, etc.

**Using `Logger` class:**

```Google Apps Script
function myFunction() {
  var name = 'World';
  Logger.log('Hello, ' + name + '!');
}
```

After running this function, view the log output by selecting "View" > "Logs" in the Script Editor menu. You should see something like:

```
[INFO] Hello, World!
```

**Using `console.log()`:**

```Google Apps Script
function anotherFunction() {
  var status = 'Running';
  console.log('The script is %s.', status);
}
```

To view these logs, you'll go to "View" > "Stackdriver Logging" or "Execution logs" for newer IDE versions. The output will look like:

```
The script is Running.
```

`console.log()` is especially handy for formatted output and complex debugging scenarios.

## Deep Dive

Historically, `Logger.log()` was the primary way to debug in Google Apps Script, but it has limitations, such as not retaining logs between executions and having to manually open the log viewer each time. With the introduction of Stackdriver Logging (now part of Google Cloud Logging) and the new IDE's `console`, Google Apps Script has aligned more with standard JavaScript practices, offering richer insights into the execution of scripts.

`console.log()` not only supports formatted strings and automatically captures the execution context (making it easier to diagnose where in your code the log came from), but it also persists logs across executions. This persistence is incredibly useful for debugging scripts that run on triggers or are executed repeatedly over time.

While `Logger` is still useful for simple, quick debug outputs and for those who prefer to keep their debugging within the Script Editor environment, `console.log()` and Google Cloud Logging are better suited for more in-depth analysis, especially when dealing with complex scripts or scripts integrating with other Google services. Through Cloud Logging, developers have access to a more robust suite of logging, monitoring, and diagnosis tools, albeit with a slightly steeper learning curve.

Considering these points, while you start with `Logger.log()` for simplicity, learning to effectively use `console.log()` and exploring the integration with Cloud Logging are valuable skills for any Google Apps Script programmer looking to debug and monitor their scripts more effectively.
