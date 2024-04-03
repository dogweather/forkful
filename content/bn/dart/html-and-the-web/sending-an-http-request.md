---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:07.022145-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart `http` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\
  \u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE HTTP \u09B8\u09AE\u09CD\u09AA\u09A6 \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\
  \u09B2\u09C0 \u098F\u09AC\u0982 \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\
  \u0995 \u0989\u09AA\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7\
  , \u098F\u099F\u09BF\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 pubspec.yaml \u09AB\
  \u09BE\u0987\u09B2\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.715040-06:00'
model: gpt-4-0125-preview
summary: "Dart `http` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u0985\u09A8\u09CD\
  \u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE\
  \ HTTP \u09B8\u09AE\u09CD\u09AA\u09A6 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u098F\u09AC\u0982 \u09B8\
  \u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995 \u0989\u09AA\u09BE\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u098F\u099F\u09BF\u0995\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u09B0 pubspec.yaml \u09AB\u09BE\u0987\u09B2\u09C7 \u09AF\u09C1\u0995\
  \u09CD\u09A4 \u0995\u09B0\u09C1\u09A8."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
Dart `http` প্যাকেজ অন্তর্ভুক্ত করে, যা HTTP সম্পদ নিয়ে কাজ করার জন্য একটি শক্তিশালী এবং সুবিধাজনক উপায়। প্রথমে, এটিকে আপনার pubspec.yaml ফাইলে যুক্ত করুন:

```yaml
dependencies:
  http: ^0.13.3
```

তারপর, আপনার Dart কোডে এটি আমদানি করুন যাতে আপনি অনুরোধ পাঠানো শুরু করতে পারেন:

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

সফল অনুরোধের জন্য নমুনা আউটপুট এরকম দেখায়:

```
Response body: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

একটি JSON বডি সহ POST অনুরোধের মতো আরো জটিল অনুরোধের জন্য, আপনি নিম্নলিখিত করবেন:

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

পোস্ট অনুরোধের জন্য নমুনা আউটপুট এরকম হতে পারে:

```
Response status: 201
Response body: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

এই উদাহরণগুলি Dart এ `http` প্যাকেজ ব্যবহার করে মৌলিক HTTP GET এবং POST অনুরোধ পাঠানো প্রদর্শন করে। এই প্যাকেজটি HTTP অনুরোধ পাঠাতে আরও জটিল পরিস্থিতিগুলিতে, যেমন হেডার এবং বডি কন্টেন্ট সহ, অধিকাংশ প্রয়োজনগুলি আবৃত করে।
