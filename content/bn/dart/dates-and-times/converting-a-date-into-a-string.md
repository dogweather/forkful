---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:39.294302-06:00
description: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09A4\u09A5\
  \u09CD\u09AF \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09AF\u09BC, \u0985\u09A5\u09AC\u09BE \u09AF\u0996\u09A8 \u0986\u09AA\
  \u09A8\u09BF \u09A1\u09C7\u099F\u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC\u2026"
lastmod: '2024-03-17T18:47:43.731380-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09A4\u09A5\
  \u09CD\u09AF \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09AF\u09BC, \u0985\u09A5\u09AC\u09BE \u09AF\u0996\u09A8 \u0986\u09AA\
  \u09A8\u09BF \u09A1\u09C7\u099F\u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC\u2026"
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কি ও কেন?

ডার্টে তারিখকে স্ট্রিং এ রূপান্তর করা একটি সাধারণ কাজ যখন আপনার মানব-পাঠযোগ্য ফর্ম্যাটে তারিখ এবং সময়ের তথ্য প্রদর্শন করতে হয়, অথবা যখন আপনি ডেটা সঞ্চয় বা সম্প্রেষণের উদ্দেশ্যে সিরিয়ালাইজ করার কথা ভাবছেন। এই প্রক্রিয়াটি তারিখ-সময়ের মানগুলির সহজ প্রকাশ এবং ব্যবহারের ক্ষেত্রে নির্ভর করে স্বনির্ধারিত করা যায় এমন একটি ফর্ম্যাটে ম্যানিপুলেশন অনুমোদন করে।

## কিভাবে:

ডার্ট তারিখ এবং সময় পরিচালনার জন্য `DateTime` ক্লাস এবং ফরম্যাটিং এর জন্য `intl` প্যাকেজ প্রদান করে। প্রথমে, আপনার `pubspec.yaml` ফাইলে `intl: ^0.17.0` (অথবা সর্বশেষ সংস্করণ) যোগ করে `intl` প্যাকেজটি থাকা নিশ্চিত করুন।

### ডার্টের কোর লাইব্রেরি ব্যবহার করে

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // আউটপুট: 2023-4-12 (উদাহরণস্বরূপ, এটি বর্তমান তারিখের উপর নির্ভর করে)
```

এই উদাহরণটি সরাসরি `DateTime` এর প্রোপার্টিজ থেকে স্ট্রিং তৈরি করে।

### `intl` প্যাকেজ ব্যবহার করে

প্রথমে, প্যাকেজটি ইমপোর্ট করুন:

```dart
import 'package:intl/intl.dart';
```

তারপরে, তারিখটি ফরম্যাট করুন:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // আউটপুট: 2023-04-12
```

`intl` প্যাকেজ অনেক বেশি জটিল ফরম্যাটিং সহজে অনুমোদন করে, যার মধ্যে লোকেল-নির্দিষ্ট ফর্ম্যাট রয়েছে:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // আউটপুট: April 12, 2023
```

এই উদাহরণগুলি দেখায় ডার্টের ডেটগুলিকে স্ট্রিং এ রূপান্তর এবং ফরম্যাট করার সহজ কিন্তু শক্তিশালী উপায় সম্পর্কে, ইহাতে ডার্টের কোর কার্যকারিতার ব্যবহার বা `intl` প্যাকেজের জন্য আরও উন্নত ফর্ম্যাটিং অপশনগুলি ব্যবহার করা হয়েছে।
