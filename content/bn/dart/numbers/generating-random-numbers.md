---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:26.300353-06:00
description: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09AF\u09BE\u09A6\u09C3\u099A\u09CD\
  \u099B\u09BF\u0995 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u098F\u09AE\
  \u09A8 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09C7\u0997\u09C1\u09B2\u09CB \u0985\u09A8\
  \u09C1\u09AE\u09BE\u09A8\u09AF\u09CB\u0997\u09CD\u09AF \u09A8\u09AF\u09BC \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\
  \u09AC\u09BE\u09B9\u09C7 \u09AD\u09BF\u09A8\u09CD\u09A8 \u09B9\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\u09AE\u09C2\u09B2\u0995\u2026"
lastmod: '2024-03-17T18:47:43.713717-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09AF\u09BE\u09A6\u09C3\u099A\u09CD\
  \u099B\u09BF\u0995 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u098F\u09AE\
  \u09A8 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09C7\u0997\u09C1\u09B2\u09CB \u0985\u09A8\
  \u09C1\u09AE\u09BE\u09A8\u09AF\u09CB\u0997\u09CD\u09AF \u09A8\u09AF\u09BC \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\
  \u09AC\u09BE\u09B9\u09C7 \u09AD\u09BF\u09A8\u09CD\u09A8 \u09B9\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\u09AE\u09C2\u09B2\u0995\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ডার্টে যাদৃচ্ছিক সংখ্যা তৈরি করার মানে হলো এমন সংখ্যাগুলি তৈরি করা যেগুলো অনুমানযোগ্য নয় এবং প্রতিটি নির্বাহে ভিন্ন হয়। প্রোগ্রামাররা পরীক্ষামূলক পরিবেশে বাস্তব দৃশ্যানুকরণ থেকে শুরু করে খেলার যান্ত্রিক সক্রিয়করণ এবং ক্রিপ্টোগ্রাফিক কার্যকলাপে যাদৃচ্ছিকতা মাধ্যমে নিরাপত্তা সুনিশ্চিত করার জন্য এই কার্যকারিতা কাজে লাগায়।

## কিভাবে:

ডার্টের কোর লাইব্রেরিতে `dart:math`-এ পাওয়া `Random` ক্লাসের মাধ্যমে যাদৃচ্ছিক সংখ্যা তৈরির সমর্থন অন্তর্ভুক্ত আছে। এখানে একটি বেসিক উদাহরণ দেওয়া হল:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // 0 এবং 99 এর মধ্যে একটি যাদৃচ্ছিক পূর্ণসংখ্যা তৈরি করে
  double randomDouble = rand.nextDouble(); // 0.0 এবং 1.0 এর মধ্যে একটি যাদৃচ্ছিক দশমিক সংখ্যা তৈরি করে
  print(randomNumber);
  print(randomDouble);
}
```

*নমুনা আউটপুট: এটি প্রতিবার নির্বাহে ভিন্ন হবে*

```
23
0.6722390975465775
```

যদি আপনি ক্রিপ্টোগ্রাফিক যাদৃচ্ছিকতার প্রয়োজন বোধ করেন, ডার্ট `Random.secure` কন্সট্রাক্টর অফার করে:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*নমুনা আউটপুট: এটি প্রতিবার নির্বাহে ভিন্ন হবে*

```
45
```

যদি আপনি Flutter প্রকল্পে কাজ করছেন বা আরও জটিল যাদৃচ্ছিকতার প্রয়োজন হয়, তাহলে নাম, ঠিকানা, এবং তারিখের মতো বিস্তৃত পরিসরের যাদৃচ্ছিক ডেটা তৈরি করতে `faker` প্যাকেজটি উপকারী হতে পারে।

`faker` ব্যবহার করতে, প্রথমে এটি আপনার `pubspec.yaml` ফাইলে যোগ করুন:

```yaml
dependencies:
  faker: ^2.0.0
```

তারপর, নিম্নলিখিতভাবে আমদানি করে এটি ব্যবহার করুন:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // একটি যাদৃচ্ছিক নাম তৈরি করে
  print(faker.address.city()); // একটি যাদৃচ্ছিক শহরের নাম তৈরি করে
}
```

*নমুনা আউটপুট:*

```
Josie Runolfsdottir
East Lysanne
```
