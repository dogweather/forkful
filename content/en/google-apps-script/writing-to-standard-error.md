---
title:                "Writing to standard error"
date:                  2024-02-01T13:42:08.099859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing to standard error"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

"Writing to standard error" in the world of Google Apps Script means directing your error messages or diagnostics to a special stream, making it easier to separate them from the standard output. Programmers do this for clarity and to aid in debugging, ensuring that error messages don't get lost in the shuffle.

## How to:

Contrary to many programming environments, Google Apps Script doesn't provide a built-in way to explicitly write to standard error directly. However, you can mimic this functionality by using `Logger.log()` for general logging or `console.log()` for more structured logging, especially when using the modern runtime that supports Stackdriver Logging.

Here’s how you’d log normally:

```Javascript
function logMessage() {
  Logger.log("This is a regular log message.");
}
```

For something closer to a standard error in Google Apps Script, you'd need to leverage the `console.error()` function, like so:

```Javascript
function logError() {
  try {
    // Simulate an error
    throw new Error("Oops, something went wrong!");
  } catch (e) {
    console.error(e.toString()); // This logs the error more visibly
  }
}
```

And here’s how you’d view these logs:
- For `Logger.log()`, you’d go to `View` > `Logs` in the Script Editor.
- For `console.error()`, you’d visit the GCP console under `Logging` in the `Reports` section of your Apps Script project.

## Deep Dive

Historically, Google Apps Script has been more focused on simplification and ease of use, integrating tightly with Google Workspace applications, than on providing low-level control over the execution environment like writing to standard error directly. The addition of `console` methods with the V8 runtime brought Google Apps Script closer to JavaScript standards, offering better structured logging options.

While `console.error()` doesn't write to "standard error" in the traditional sense, as there's no direct access to the process's STDERR in Google Apps Script, it does allow you to categorize messages as errors within the logging infrastructure of Google Cloud's logging, making it a valuable tool for debugging and monitoring applications.

If you find yourself often needing to distinguish between standard output and error messages in a more granular manner, it might indicate that your project could benefit from incorporation of more robust development practices or tools. This might include using third-party logging services, or for complex applications, considering development in a more traditional environment that offers finer control over input and output streams.
