---
title:                "Working with JSON"
date:                  2024-02-01T13:42:05.224919-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON, or JavaScript Object Notation, is a lightweight format for storing and transporting data, often used when data is sent from a server to a web page. Programmers use JSON because it's easy to read and write for humans, and easy to parse and generate for machines, making it a go-to for web applications, including those you develop with Google Apps Script.

## How to:

### Parsing JSON

Let's say you've got a simple JSON string you want to work with in Google Apps Script. Here's how to parse it:

```Google Apps Script
var jsonString = '{"name": "Alex", "age": 30, "city": "New York"}';
var jsonObj = JSON.parse(jsonString);
Logger.log(jsonObj.name); // Logs: Alex
```

### Stringify JavaScript Object

Conversely, if you have a JavaScript object and you want to convert it into a JSON string, you do the opposite:

```Google Apps Script
var user = {name: "Alex", age: 30, city: "New York"};
var jsonString = JSON.stringify(user);
Logger.log(jsonString); // Logs: {"name":"Alex","age":30,"city":"New York"}
```

### Fetch JSON from an API

Google Apps Script shines when it comes to interacting with APIs to get JSON data. Here’s a simple example using the `UrlFetchApp` service to get data from a mock API:

```Google Apps Script
var response = UrlFetchApp.fetch('https://jsonplaceholder.typicode.com/users/1');
var json = JSON.parse(response.getContentText());
Logger.log(json.name); // Depending on the API's response, e.g., "Leanne Graham"
```

## Deep Dive

Historically, before JSON became the juggernaut in data interchange formats, XML was the go-to choice. However, JSON's minimalism and ease of use gave it an edge, leading to widespread adoption in web services and applications. In Google Apps Script, particularly, managing JSON is a cakewalk thanks to the robust `JSON` global object which provides methods like `parse` and `stringify` right out of the box.

While Google Apps Script is fantastic for rapidly deploying applications that integrate with Google Services, it's important to remember that it runs on Google's servers. This means any interaction with external APIs using `UrlFetchApp` or manipulation of large JSON payloads could be subject to Google's quotas and limitations. For heavy-duty or high-performance applications, considering a more dedicated backend setup might be worthwhile. Still, for the vast number of use cases, especially those involving Google Workspace data, Google Apps Script's JSON handling capabilities are more than sufficient.
