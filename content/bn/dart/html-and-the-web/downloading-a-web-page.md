---
title:                "একটি ওয়েবপেজ ডাউনলোড করা"
date:                  2024-03-17T17:47:18.404601-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি ওয়েব পেজ ডাউনলোড করার মানে হল এর URL মাধ্যমে ওয়েব পেজের কন্টেন্ট আনানো যাতে তা প্রক্রিয়াকরণ অথবা সংরক্ষণের জন্য ব্যবহার করা যায়। প্রোগ্রামাররা তথ্য উত্তোলন, পরিবর্তন নজরদারি, বা কন্টেন্ট আর্কাইভ করার উদ্দেশ্যে এটি করে থাকেন, যা ওয়েব স্ক্র্যাপিং, ডাটা মাইনিং, এবং স্বয়ংক্রিয় পরীক্ষণ কাজে একটি মৌলিক উপাদান।

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
