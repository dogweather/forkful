---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:55.250149-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \u09C7, \u0986\u09AA\u09A8\u09BF `http` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 HTTP \u0985\u09A8\
  \u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8 \u09AF\u09BE \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\
  \u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0986\
  \u09B8\u09C7\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\
  \u09B0 `pubspec.yaml` \u09AB\u09BE\u0987\u09B2\u09C7 `http`\u2026"
lastmod: '2024-03-17T18:47:43.718272-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7, \u0986\u09AA\u09A8\u09BF `http` \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\
  \u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09BE \u09AC\u09C7\u09B8\
  \u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0986\u09B8\u09C7\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 `pubspec.yaml` \u09AB\u09BE\u0987\u09B2\u09C7\
  \ `http` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF \u09AF\u09CB\u0997\
  \ \u0995\u09B0\u09C1\u09A8."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
ডার্টে, আপনি `http` প্যাকেজ ব্যবহার করে HTTP অনুরোধ পাঠাতে পারেন যা বেসিক প্রমাণীকরণের সাথে আসে। প্রথমে, আপনার `pubspec.yaml` ফাইলে `http` প্যাকেজটি যোগ করুন:

```yaml
dependencies:
  http: ^0.13.4
```
তারপর, আপনার ডার্ট ফাইলে প্যাকেজটি ইম্পোর্ট করুন:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

বেসিক প্রমাণীকরণ সহ একটি GET অনুরোধ পাঠাতে, আপনি নিম্নলিখিত কোড ব্যবহার করতে পারেন:

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

এই কোড 'https://yourapi.com/userdata' এ একটি GET অনুরোধ পাঠায়, বেসিক প্রমাণীকরণের শিরোনাম সহ। ব্যবহারকারী নাম এবং পাসওয়ার্ড বেস64 এ কোডেড এবং বেসিক এক্সেস প্রমাণীকরণ মানদণ্ড অনুযায়ী 'Authorization' শিরোনামে পাস করা হয়।

**নমুনা আউটপুট:**

সফল অনুরোধ এবং সার্ভার যদি একটি স্থিতি কোড 200 ফিরিয়ে আসে, আপনি দেখতে পাবেন:

```plaintext
User data fetched successfully!
Response body: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

যদি প্রমাণীকরণ ব্যর্থ হয় অথবা অন্য কোনও ত্রুটি ঘটে, তবে রেসপন্স স্থিতি কোড সমস্যাটি চিহ্নিত করতে সাহায্য করবে।
