---
date: 2024-02-01 21:12:43.663761-07:00
description: "Logging in programming involves recording events, errors, or notable\
  \ occurrences during runtime. Programmers do it to debug issues, monitor performance,\u2026"
lastmod: '2024-03-13T22:44:59.675755-06:00'
model: gpt-4-0125-preview
summary: "Logging in programming involves recording events, errors, or notable occurrences\
  \ during runtime. Programmers do it to debug issues, monitor performance,\u2026"
title: Logging
---

{{< edit_this_page >}}

## What & Why?

Logging in programming involves recording events, errors, or notable occurrences during runtime. Programmers do it to debug issues, monitor performance, and keep a record of operational data, making it pivotal for maintaining and understanding the behavior of software in production.

## How to:

In Google Apps Script, logging can be performed using various methods, such as the `Logger` class and `console.log()`. The Logger class is the traditional way, suited for simple debugging and development purposes. As of recent updates, `console.log()` offers more flexibility and integration with Stackdriver Logging, providing a more robust solution for monitoring your Apps Scripts in Google Cloud Platform.

**Using Logger:**

```javascript
function logSample() {
  Logger.log('This is a simple log message');
  
  var value = 5;
  Logger.log('The value is: %s', value); // String formatting
}

// To view the log:
// 1. Run the logSample function.
// 2. View -> Logs
```

**Sample Logger Output:**

```
[22-04-20 10:00:00:000 PDT] This is a simple log message
[22-04-20 10:00:00:001 PDT] The value is: 5
```

**Using console.log():**

```javascript
function consoleLogSample() {
  console.log('This message goes to Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Logging an object:', obj);
}

// Logs can be viewed in the Google Cloud Platform (GCP) console under Stackdriver Logging
```

**Sample console.log() Output:**

```
This message goes to Stackdriver Logging
Logging an object: {name: "Jane", role: "Developer"}
```

By transitioning to `console.log()` for complex applications, developers can efficiently parse and analyze logs using the powerful filters and tools provided by GCP, which is not as straightforward with the traditional Logger class.

## Deep Dive:

Logging in Google Apps Script has evolved significantly. Initially, the `Logger` class was the primary method for developers to debug their scripts. It's simple and sufficient for basic scripts, but it lacks the capabilities needed for modern cloud applications, such as searching logs or analyzing log trends over time.

The introduction of `console.log()` bridged this gap by integrating Google Apps Script logging with Google Cloud's Stackdriver Logging (now called Operations Suite), providing a centralized platform for logging, monitoring, and debugging applications. This not only allowed logging at scale but also opened up advanced log management features like log-based metrics, real-time log analysis, and integration with other Google Cloud services.

While `Logger` still serves a purpose for quick debugging and logging in smaller scripts, the evolution towards using `console.log()` reflects a broader shift in developing scalable, cloud-native applications. It underscores Google's commitment to providing developers with tools that cater to the complexity and scale of today's applications. However, newcomers should be aware of the slightly steeper learning curve and the necessity to familiarize themselves with Google Cloud Platform concepts. Despite this, the move is advantageous for developers looking to leverage cloud capabilities fully. This alignment with cloud services is part of a wider trend in software development, emphasizing the importance of robust, scalable logging mechanisms in the era of cloud computing.
