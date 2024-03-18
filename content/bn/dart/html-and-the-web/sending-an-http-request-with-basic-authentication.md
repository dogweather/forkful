---
title:                "বেসিক অথেন্টিকেশন সহ HTTP রিকুয়েস্ট প্রেরণ"
date:                  2024-03-17T18:18:55.250149-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP অনুরোধ পাঠানো সহ বেসিক প্রমাণীকরণে একটি ব্যবহারকারীর পরিচয় যাচাই করতে একটি অনুরোধের সাথে একটি ব্যবহারকারী নাম এবং পাসওয়ার্ড সংযুক্ত করার প্রক্রিয়া জড়িত। প্রোগ্রামাররা প্রমাণীকরণ দাবি করে এমন সম্পদ অ্যাক্সেস করার জন্য এটি ব্যবহার করে, যা ক্লায়েন্ট এবং সার্ভারের মধ্যে নিরাপদ যোগাযোগ নিশ্চিত করে।

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
