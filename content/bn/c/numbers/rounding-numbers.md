---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:25.157860-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09C1\u09A8\u09B0\u09CD\u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u0985\u0999\u09CD\u0995\u0997\u09C1\u09B2\u09BF\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0995\u09BF\u099B\u09C1\
  \ \u09A8\u09BF\u09AF\u09BC\u09AE \u0985\u09A8\u09C1\u09AF\u09BE\u09AF\u09BC\u09C0\
  \ \u09A4\u09BE\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\u09A4\u09BE \u0995\
  \u09AE\u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE, \u098F\u099F\u09BF \u09B9\
  \u09AF\u09BC \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09A6\u09BF\u0995\u09C7 \u09AC\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:44.536145-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09C1\u09A8\u09B0\u09CD\u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u0985\u0999\u09CD\u0995\u0997\u09C1\u09B2\u09BF\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0995\u09BF\u099B\u09C1\
  \ \u09A8\u09BF\u09AF\u09BC\u09AE \u0985\u09A8\u09C1\u09AF\u09BE\u09AF\u09BC\u09C0\
  \ \u09A4\u09BE\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\u09A4\u09BE \u0995\
  \u09AE\u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE, \u098F\u099F\u09BF \u09B9\
  \u09AF\u09BC \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09A6\u09BF\u0995\u09C7 \u09AC\u09BE\
  \u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
---

{{< edit_this_page >}}

## কি এবং কেন?

সংখ্যা পুনর্নির্ণয় হল একটি সংখ্যার অঙ্কগুলি নির্দিষ্ট কিছু নিয়ম অনুযায়ী তার নির্ভুলতা কমিয়ে দেওয়ার প্রক্রিয়া, এটি হয় নিকটতম পূর্ণ সংখ্যার দিকে বা নির্দিষ্ট সংখ্যক দশমিক স্থানের দিকে করা হয়। প্রোগ্রামাররা এটি করে থাকেন বিভিন্ন কারণে, যা স্টোরেজের পরিমাণ সীমিত করা থেকে শুরু করে, ব্যবহারকারীর জন্য আউটপুট সরলীকরণ, অথবা খুব সামান্য পরিবর্তনে সংবেদনশীল সঠিক গণিতীয় অপারেশন নিশ্চিত করা পর্যন্ত।

## কিভাবে:

C প্রোগ্রামিং ভাষায় সংখ্যা পুনর্নির্ণয় বিভিন্ন ফাংশন ব্যবহার করে সম্পন্ন করা যেতে পারে, কিন্তু সবচেয়ে সাধারণ পদ্ধতি হল `floor()`, `ceil()`, এবং `round()` ফাংশনগুলি ব্যবহার করা। এই ফাংশনগুলি শান্ত্রীয় গণিত লাইব্রেরির অংশ, তাই আপনাকে `math.h` আপনার প্রোগ্রামে অন্তর্ভুক্ত করতে হবে।

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // নীচের দিকে পুনর্নির্ণয় করার জন্য floor() ব্যবহার করা হয়
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // উপরের দিকে পুনর্নির্ণয় করার জন্য ceil() ব্যবহার করা হয়
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // নিকটতম পূর্ণ সংখ্যায় পুনর্নির্ণয় করার জন্য round() ব্যবহার করা হয়
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // নির্দিষ্ট সংখ্যক দশমিক স্থানে পুনর্নির্ণয় করার জন্য গুণ এবং ভাগ করা জড়িত
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("রেখে দুই দশমিক স্থানে পুনর্নির্ণয়: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

আউটপুট:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
দুই দশমিক স্থানে পুনর্নির্ণয়: 9.53
```

## গভীর ডুব

সংখ্যা পুনর্নির্ণয়ের গণিত এবং গণনা বিদ্যায় গভীর ইতিহাস রয়েছে, যা তাত্ত্বিক এবং প্রয়োগমূলক উভয় দিকেই অভিন্ন। C ভাষায়, `floor()`, `ceil()`, এবং `round()` মৌলিক ফাংশনালিটি প্রদান করে থাকলেও, ফ্লোটিং পয়েন্ট সংখ্যাগুলি অথবা নির্দিষ্ট দশমিক স্থানে পুনর্নির্ণয়ের মৌলিক সারাংশ আরও জটিল হয়ে যায় বাইনারি প্রতিনিধিত্বের কারণে। এই প্রতিনিধিত্ব বাইনারিতে যথাযথভাবে প্রকাশ করা না যায় এমন সংখ্যা (যেমন 0.1) গুলি সম্পর্কে আচরণের কারণে অপ্রত্যাশিত ফলাফলের দিকে নিয়ে যেতে পারে।

এই ফাংশনগুলি C স্ট্যান্ডার্ড লাইব্রেরিতে সজ্জিত, `<math.h>` এ সংজ্ঞায়িত। সংখ্যা পুনর্নির্ণয়ের সময়, বিশেষ করে আর্থিক অথবা নির্ভুল ইঞ্জিনিয়ারিং হিসাবের জন্য, বাইনারি ফ্লোটিং-পয়েন্ট সংখ্যাগুলি ব্যবহারের প্রভাব বিবেচনা করা আবশ্যক। অত্যন্ত নির্ভুল বা দশমিক-নির্দিষ্ট পুনর্নির্ণয়ের জন্য C ফাংশনগুলির বিকল্প হিসেবে কাস্টম রাউন্ডিং ফাংশনগুলি বা যেমন GMP বা MPFR এর মত অসীম-নির্ভুলতা অঙ্কনশাস্ত্র লাইব্রেরিগুলি ব্যবহার করা যেতে পারে, যদিও এগুলি অতিরিক্ত জটিলতা এবং নির্ভরতা আনে।

ব্যবহারিকভাবে, C তে পুনর্নির্ণয়ের সঠিক পদ্ধতি নির্বাচন করা অর্থাৎ নির্ভুলতা, কর্মক্ষমতা, এবং বাস্তবিকতার মধ্যে সামঞ্জস্য স্থাপন করা যার জন্য যে অ্যাপ্লিকেশন বিকাশিত হচ্ছে তার বিষয়বস্তু-নির্দিষ্ট দাবিদাওয়াগুলির গভীর বোঝাপড়া প্রয়োজন।
