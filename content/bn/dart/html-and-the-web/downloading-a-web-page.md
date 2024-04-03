---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:18.404601-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u098F\u0995\u099F\u09BF\
  \ \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A5\u09BE\u09B0\u09CD\u09A1\
  -\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 `http` \u09AA\u09CD\u09AF\u09BE\
  \u0995\u09C7\u099C \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\
  \u09BE HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C \u09A1\
  \u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF\u2026"
lastmod: '2024-03-17T18:47:43.717216-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\
  \u09BC \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7\
  \ `http` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09AC\
  \u09C7\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
Dart একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি হিসেবে `http` প্যাকেজ প্রদান করে, যা HTTP অনুরোধ সম্পাদনের জন্য। এখানে একটি ওয়েবপেজ ডাউনলোড করার জন্য এটি ব্যবহার করার একটি বেসিক উদাহরণ দেওয়া হল:

প্রথমে, `pubspec.yaml` এ `http` প্যাকেজ যোগ করুন:

```yaml
dependencies:
  http: ^0.13.3
```

তারপরে, প্যাকেজটি ইম্পোর্ট করুন এবং এটি ব্যবহার করে একটি ওয়েব পেজের কন্টেন্ট ফেচ করুন:

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

**নমুনা আউটপুট** (এটি ওয়েব পেজের কন্টেন্টের উপর ভিত্তি করে ভিন্ন হবে):

```
Page downloaded:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

কুকি হ্যান্ডলিং বা ইউজার-এজেন্ট হেডার সেট করার মতো জটিল সিনারিওর জন্য, আপনি একই `http` প্যাকেজ ব্যবহার করবেন কিন্তু আপনার অনুরোধে অতিরিক্ত কনফিগারেশন যোগ করবেন:

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

এরকম হেডার ব্যবহার করে ব্রাউজারের অনুরোধ আরও নির্ভুলভাবে অনুকরণ করা সম্ভব হয়, যা স্ক্র্যাপিং বিরোধী নির্দিষ্ট দাবি বা সুরক্ষা সহ সাইটগুলোর সাথে মোকাবেলা করতে বিশেষভাবে উপকারী।
