---
date: 2024-03-08 21:33:39.871728-07:00
description: "How to: Dart provides the `http` package, a popular third-party library\
  \ for making HTTP requests. Here\u2019s a basic example of how to use it to download\
  \ a\u2026"
lastmod: '2024-03-13T22:44:59.819698-06:00'
model: gpt-4-0125-preview
summary: Dart provides the `http` package, a popular third-party library for making
  HTTP requests.
title: Downloading a web page
weight: 42
---

## How to:
Dart provides the `http` package, a popular third-party library for making HTTP requests. Here’s a basic example of how to use it to download a webpage:

First, add the `http` package to your `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Then, import the package and use it to fetch the content of a web page:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Page downloaded:');
    print(response.body);
  } else {
    print('Request failed with status: ${response.statusCode}.');
  }
}
```

**Sample output** (this will vary based on the content of the web page):

```
Page downloaded:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

For more complex scenarios, like handling cookies or setting user-agent headers, you would use the same `http` package but with additional configurations to your request:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Page downloaded with custom headers:');
    print(response.body);
  } else {
    print('Request failed with status: ${response.statusCode}.');
  }
}
```

Using headers like these can mimic browser requests more accurately, which is particularly useful when dealing with sites that have specific requirements or protections against scraping.
