---
title:                "Logging"
date:                  2024-02-01T13:42:16.990903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logging"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/logging.md"
---

{{< edit_this_page >}}

## What & Why?

Logging in Google Apps Script is all about keeping tabs on what your script is doing and when it's doing it. Why bother? Because when things go sideways, and they sometimes will, these logs are your detective toolkit for figuring out what went wrong.

## How to:

Google Apps Script offers a straightforward way to log messages using the `Logger` class. Here's a quick spin through how you can add logging to your script:

```Javascript
function myFunction() {
  Logger.log('This is a log message'); // Logs a simple text message.

  var value = 'test value';
  Logger.log('The value is: %s', value); // Logs with string formatting.

  // You can view logs by going to View > Logs in the Script Editor.
}
```

After running `myFunction`, you check the log output by navigating through the Script Editor menu: **View > Logs**. You'd see something like this:

```
[22-02-01 10:00:00:000 PST] This is a log message
[22-02-01 10:00:00:001 PST] The value is: test value
```

Pretty neat, right? But what if your script is a web app or bound to a Google Sheet and you want to keep logs persistently?

For more persistent logging, consider using `Console.log()` in combination with Stackdriver Logging:

```Javascript
function myWebAppFunction() {
  console.log('This logs to Stackdriver Logging for more persistent storage.');
  // Access Stackdriver logs via Apps Script Dashboard > My Executions.
}
```

This way, your logs are stored in a more permanent, searchable database, accessible through the Google Cloud Platform.

## Deep Dive

Before the introduction of `console.log()`, Google Apps Script developers relied solely on `Logger.log()` for debugging. While it gets the job done for quick and dirty logging, it wasn't meant for more complex scenarios, such as long-term storage or analytical purposes. This is where the more recent `console.log()` shines, bridging the gap between simplicity and the need for more robust logging options.

However, it's essential to understand that while Stackdriver/Google Cloud Logging provides an extensive logging system, it might be overkill for small projects or scripts. The choice between `Logger.log()` and `console.log()` ultimately comes down to your project's needs: use `Logger` for simplicity and `console` for depth.

Additionally, for debugging purposes, Google Apps Script recently integrated with Google Cloud's Error Reporting. This offers a more granular error tracking mechanism, further enhancing the developer's ability to monitor and troubleshoot scripts efficiently.

Though Google Apps Script's logging mechanisms are fairly basic, they are part of a larger ecosystem within Google Cloud Platform, offering paths to more sophisticated logging and error tracking systems. As you grow your applications, keep in mind these more advanced options for future implementation.
