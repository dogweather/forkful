---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:07.022145-06:00
description: "Dart \u098F HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB \u09B9\u09B2\u09CB Dart \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8 \u09A5\u09C7\u0995\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u09AC\
  \u09BE API \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\
  \u0997 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\
  \u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u0986\u09A8\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.715040-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB \u09B9\u09B2\u09CB Dart \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8 \u09A5\u09C7\u0995\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u09AC\
  \u09BE API \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\
  \u0997 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\
  \u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u0986\u09A8\u09BE\u09B0\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Dart এ HTTP অনুরোধ পাঠানো হলো Dart অ্যাপ্লিকেশন থেকে একটি ওয়েব সার্ভার বা API এর সাথে যোগাযোগ শুরু করার প্রক্রিয়া। প্রোগ্রামাররা ওয়েব থেকে ডাটা আনার জন্য, ফর্ম জমা দেওয়ার জন্য, এবং RESTful সেবাগুলির সাথে যোগাযোগ করার জন্য এটি করে থাকেন, যা Dart এ ওয়েব, সার্ভার-সাইড, এবং মোবাইল অ্যাপ্লিকেশন ডেভেলপমেন্ট এর জন্য একটি মৌলিক ক্রিয়াকলাপ।

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
