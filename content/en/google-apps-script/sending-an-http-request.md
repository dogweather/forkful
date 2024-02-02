---
title:                "Sending an HTTP request"
date:                  2024-02-01T13:42:12.621447-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sending an HTTP request"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means asking the web to spit back some data or do something for you, often by accessing APIs or web pages. Programmers do it to integrate their Google Apps Script projects with external services, scraping web data, or automating interactions with websites.

## How to:

Google Apps Script makes sending HTTP requests pretty straightforward with the `UrlFetchApp` class. Here's a quick example on how to get data from a mock API:

```Javascript
function fetchSomeData() {
  var response = UrlFetchApp.fetch("https://jsonplaceholder.typicode.com/posts/1");
  var data = JSON.parse(response.getContentText());
  Logger.log(data);
}
```

Running this, you'll see in your logs something like:

```
{userId=1, id=1, title="sunt aut facere repellat...", body="quia et suscipit\nsuscipit..."}
```

To send a POST request, say to send data to an external API, you can modify the request like this:

```Javascript
function postData() {
  var data = {
    'title': 'foo',
    'body': 'bar',
    'userId': 1
  };
  
  var options = {
    'method': 'post',
    'contentType': 'application/json',
    // Convert the JavaScript object to a JSON string.
    'payload': JSON.stringify(data)
  };
  
  var response = UrlFetchApp.fetch('https://jsonplaceholder.typicode.com/posts', options);
  Logger.log(JSON.parse(response.getContentText()));
}
```

On execution, the Logger will show you the response from the server, including the ID of the new resource you've just created. Pretty neat, eh?

## Deep Dive

Google Apps Script's `UrlFetchApp` class is a relatively straightforward way to make HTTP requests, making it super approachable for new programmers and perfectly sufficient for many use-cases. It's worth noting, though, that in the broader programming ecosystem, you might work with more complex libraries or frameworks for HTTP requests, such as Axios or Fetch API in JavaScript for more advanced needs, like error handling or intercepting requests.

As of the "current" state of Google Apps Script, `UrlFetchApp` doesn't support the modern `async/await` syntax directly, meaning you're working in a more synchronous-looking way even though the underlying operation is network-based and essentially asynchronous. This isn't usually a limitation for simple scripts or automations but might become relevant for more complex applications or when trying to implement specific patterns of asynchronous programming. 

Historically, `UrlFetchApp` has evolved to support more HTTP features over time, including setting headers, method (GET, POST, etc.), and payload, making it flexible for most HTTP requests you'd need to make from your Google Apps Script projects. Nonetheless, always check the current limitations and quotas Google imposes to ensure your script runs smoothly without hitting any unexpected roadblocks.
