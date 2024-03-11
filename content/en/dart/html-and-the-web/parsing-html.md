---
date: 2024-03-08 21:33:35.846788-07:00
description: "Parsing HTML in programming involves extracting data from HTML documents.\
  \ Programmers do this to interact with or scrape web content for information\u2026"
lastmod: '2024-03-11T00:14:33.675570-06:00'
model: gpt-4-0125-preview
summary: "Parsing HTML in programming involves extracting data from HTML documents.\
  \ Programmers do this to interact with or scrape web content for information\u2026"
title: Parsing HTML
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML in programming involves extracting data from HTML documents. Programmers do this to interact with or scrape web content for information extraction, testing, or automation purposes, even when official APIs are not available.

## How to:
Dart does not provide built-in support for HTML parsing in its core libraries. However, you can use a third-party package like `html` to parse and manipulate HTML documents. 

First, add the `html` package to your `pubspec.yaml` file:

```yaml
dependencies:
  html: ^0.15.0
```

Then, import the package into your Dart file:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Here's a basic example of parsing a string containing HTML and extracting data:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hello, Dart!</h1>
      <p>This is a paragraph in a sample HTML</p>
    </body>
  </html>
  """;

  // Parse the HTML string
  Document document = parse(htmlDocument);

  // Extracting data
  String title = document.querySelector('h1')?.text ?? "No title found";
  String paragraph = document.querySelector('p')?.text ?? "No paragraph found";

  print('Title: $title');
  print('Paragraph: $paragraph');
}
```

Output:

```
Title: Hello, Dart!
Paragraph: This is a paragraph in a sample HTML
```

To interact with real-world web pages, you might combine `html` parsing with HTTP requests (using `http` package to fetch web content). Here's a quick example:

First, add the `http` package along with `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Then, fetch and parse an HTML page from the web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Fetch the webpage
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Assume the page has <h1> tags you're interested in
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Headlines: $headlines');
  } else {
    print('Request failed with status: ${response.statusCode}.');
  }
}
```

Note: The web scraping technique shown above should be used responsibly and in compliance with the website’s terms of service.
