---
date: 2024-03-08 21:33:34.209785-07:00
description: "Sending an HTTP request with basic authentication involves attaching\
  \ a username and password to a request to verify the user's identity. Programmers\
  \ use\u2026"
lastmod: '2024-03-13T22:44:59.820560-06:00'
model: gpt-4-0125-preview
summary: "Sending an HTTP request with basic authentication involves attaching a username\
  \ and password to a request to verify the user's identity. Programmers use\u2026"
title: Sending an HTTP request with basic authentication
weight: 45
---

## What & Why?

Sending an HTTP request with basic authentication involves attaching a username and password to a request to verify the user's identity. Programmers use it to access resources that require authentication, ensuring secure communication between the client and server.

## How to:

In Dart, you can use the `http` package to send HTTP requests with basic authentication. First, add the `http` package to your `pubspec.yaml` file:

```yaml
dependencies:
  http: ^0.13.4
```

Then, import the package in your Dart file:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

To send a GET request with basic authentication, you can use the following code:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('User data fetched successfully!');
    print('Response body: ${response.body}');
  } else {
    print('Failed to fetch user data with status code: ${response.statusCode}');
  }
}
```

This code sends a GET request to 'https://yourapi.com/userdata' with a basic authentication header. The username and password are encoded in base64 and passed in the 'Authorization' header as per the basic access authentication standards.

**Sample output:**

Upon successful request and if the server returns a status code of 200, you might see:

```plaintext
User data fetched successfully!
Response body: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

If authentication fails or there is any other error, the response status code will help identify the issue.
