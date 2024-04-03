---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:46.654290-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart-\u098F, \u0986\u09AA\u09A8\
  \u09BF `String` \u09B6\u09CD\u09B0\u09C7\u09A3\u09C0\u09B0 \u09A6\u09C7\u0993\u09DF\
  \u09BE `toLowerCase()` \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\
  \u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u098F\u0987 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u098F\u0995\
  \u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09AB\u09C7\u09B0\u09A4\u2026"
lastmod: '2024-03-17T18:47:43.703040-06:00'
model: gpt-4-0125-preview
summary: "Dart-\u098F, \u0986\u09AA\u09A8\u09BF `String` \u09B6\u09CD\u09B0\u09C7\u09A3\
  \u09C0\u09B0 \u09A6\u09C7\u0993\u09DF\u09BE `toLowerCase()` \u09AA\u09A6\u09CD\u09A7\
  \u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\
  \u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0987 \u09AA\
  \u09A6\u09CD\u09A7\u09A4\u09BF \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AB\u09C7\u09B0\u09A4 \u09A6\u09C7\
  \u09DF \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09B8\u09AE\u09B8\u09CD\u09A4 \u0986\
  \u09AA\u09BE\u09B0\u0995\u09C7\u09B8 \u0985\u0995\u09CD\u09B7\u09B0 \u09B2\u09CB\
  \u09DF\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u09B9\
  \u09DF\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09C7\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u099F\u09BF \u09A6\u09C7\u0996\u09BF."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Dart-এ, আপনি `String` শ্রেণীর দেওয়া `toLowerCase()` পদ্ধতি ব্যবহার করে একটি স্ট্রিংকে লোয়ারকেসে পরিণত করতে পারেন। এই পদ্ধতি একটি নতুন স্ট্রিং ফেরত দেয় যেখানে সমস্ত আপারকেস অক্ষর লোয়ারকেসে পরিণত হয়। চলুন একটি সাধারণ উদাহরণের মাধ্যমে এটি দেখি:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();
  
  print(lowerCaseString);  // আউটপুট: hello, world!
}
```

Dart লোয়ারকেসে পরিবর্তনের মতো বেসিক স্ট্রিং ম্যানিপুলেশন কার্যগুলির জন্য বাহ্যিক লাইব্রেরি প্রয়োজন করে না, কারণ স্ট্যান্ডার্ড লাইব্রেরির `String` শ্রেণী বেশ ব্যাপক। তবে, লোকাল স্পেসিফিক নিয়ম সম্পর্কে জড়িত আরও জটিল ম্যানিপুলেশনের জন্য, আপনি `intl` প্যাকেজ বিবেচনা করতে পারেন, যা আন্তর্জাতিকীকরণ এবং স্থানীয়করণের সাহায্যসহ, লোকালের ভিত্তিতে কেস পরিবর্তনের সুবিধা প্রদান করে:

`intl` ব্যবহার করতে, আপনার `pubspec.yaml` ফাইলে এটি যুক্ত করুন:

```yaml
dependencies:
  intl: ^0.17.0
```

তারপর, আপনি বিশেষ লোকালের উপর ভিত্তি করে স্ট্রিংকে লোয়ারকেসে পরিণত করতে `toLocaleLowerCase()` পদ্ধতি ব্যবহার করতে পারেন:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // তুর্কি লোকাল
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // আউটপুট: istanbul
  
  // ডিফল্ট লোকাল (en)
  print(originalString.toLowerCase()); // আউটপুট: i̇stanbul
}
```

এই উদাহরণে, লক্ষ্য করুন কিভাবে তুর্কি লোকাল বিন্দুবিহীন 'i' কে সঠিকভাবে সম্পাদন করে, যা আন্তর্জাতিকীকৃত অ্যাপ্লিকেশনগুলিতে লোকাল-সচেতন পরিবর্তনের গুরুত্ব দেখায়।
