---
date: 2024-03-08 21:33:45.306454-07:00
description: "Sending an HTTP request in Dart is the process of initiating communications\
  \ with a web server or API from a Dart application. Programmers do it to fetch\u2026"
lastmod: '2024-03-11T00:14:33.674707-06:00'
model: gpt-4-0125-preview
summary: "Sending an HTTP request in Dart is the process of initiating communications\
  \ with a web server or API from a Dart application. Programmers do it to fetch\u2026"
title: Sending an HTTP request
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request in Dart is the process of initiating communications with a web server or API from a Dart application. Programmers do it to fetch data from the web, submit forms, and interact with RESTful services, making it a fundamental operation for web, server-side, and mobile application development in Dart.

## How to:

Dart includes the `http` package, a powerful and convenient way to work with HTTP resources. First, include it in your pubspec.yaml file:

```yaml
dependencies:
  http: ^0.13.3
```

Then, import it in your Dart code to start making requests:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Response body: ${response.body}');
  } else {
    print('Request failed with status: ${response.statusCode}.');
  }
}
```

Sample output for a successful request could look like this:

```
Response body: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

For more complex requests, such as POST requests with a JSON body, you would do the following:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('Response status: ${response.statusCode}');
    print('Response body: ${response.body}');
  } else {
    print('Failed to create a new post. Status: ${response.statusCode}');
  }
}
```

Sample output for the post request might be:

```
Response status: 201
Response body: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

These examples showcase basic HTTP GET and POST requests using the `http` package in Dart. This package covers most needs for sending HTTP requests, including more complex scenarios with headers and body content.
