---
title:                "Parsing HTML"
date:                  2024-02-01T13:42:06.923943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML in Google Apps Script is about extracting specific data from a chunk of HTML content. Programmers do it to automate tasks such as scraping web content, processing form data, or integrating with web-based APIs that return HTML instead of JSON.

## How to:

Google Apps Script does not natively support DOM manipulation like in-browser JavaScript does, due to its server-side nature. However, we can use the `XmlService` to parse and traverse HTML, though it’s a bit like taking a detour. Here's a simple example to extract data from an HTML string:

```Javascript
function parseHtmlExample() {
  var html = '<html><head><title>Test Page</title></head><body><p>Hello, world!</p></body></html>';
  var document = XmlService.parse(html);
  var rootElement = document.getRootElement();
  
  // Navigate the HTML structure
  var body = rootElement.getChild('body');
  var paragraph = body.getChild('p');
  var content = paragraph.getText();
  
  Logger.log(content); // Outputs: Hello, world!
}
```

In cases where you're dealing with malformed HTML (which is common on the web), this approach might not work as expected. For more robust HTML parsing, you might consider fetching the content and then processing it with a regular expression or looking for libraries capable of handling HTML parsing more gracefully, though the latter option isn't straightforward in Google Apps Script.

## Deep Dive

Parsing HTML with `XmlService` in Google Apps Script is somewhat archaic and can be quite challenging, especially for complex or poorly formatted HTML. The service expects well-formed XML, which can lead to errors or unexpected behavior when dealing with actual web pages that often don't meet XML standards.

Historically, before the advent of more advanced client-side JavaScript and APIs returning data in formats like JSON, parsing HTML was a more common requirement. Today, while still useful in certain scenarios, there are better alternatives for most applications. For instance, many web services now offer APIs returning JSON, which is much simpler to handle in Google Apps Script using the `UrlFetchApp` and native JSON parsing with `JSON.parse()`.

However, when you're stuck needing to parse HTML, consider your approach carefully. For simple tasks, the `XmlService` might suffice, but always validate and sanitize HTML input to avoid errors. For more complex HTML or dynamic content (like that generated through JavaScript), you might need to use other tools or services outside of Google Apps Script to preprocess the HTML into a more manageable format before parsing.
