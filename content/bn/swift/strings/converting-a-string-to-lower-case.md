---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:14.568492-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u0989\u09AA\u09B0\u09C7\u09B0 \u0995\u09C7\u09B8\u09C7\u09B0 \u0985\
  \u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8 \u09B8\u09AE\u09A4\
  \u09C1\u09B2\u09CD\u09AF\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4 \u09A7\u09BE\u09B0\u09BE\u09AC\u09BE\u09B9\u09BF\u0995\u09A4\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.395827-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u0989\u09AA\u09B0\u09C7\u09B0 \u0995\u09C7\u09B8\u09C7\u09B0 \u0985\
  \u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8 \u09B8\u09AE\u09A4\
  \u09C1\u09B2\u09CD\u09AF\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4 \u09A7\u09BE\u09B0\u09BE\u09AC\u09BE\u09B9\u09BF\u0995\u09A4\u09BE,\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি স্ট্রিংকে লোয়ারকেসে রূপান্তরিত করা মানে উপরের কেসের অক্ষরগুলিকে তাদের লোয়ারকেস সমতুল্যে পরিবর্তন করা। প্রোগ্রামাররা এটি সাধারণত ধারাবাহিকতা, প্রায়ই কেস-অসংবেদনশীল তুলনাগুলি বা টেক্সট ইনপুট স্ট্যান্ডার্ডাইজের জন্য করে।

## কিভাবে:

Swift এটিকে `lowercased` নামে একটি প্রপার্টির মাধ্যমে সহজ করে তুলেছে। এখানে আপনি কিভাবে এটি ব্যবহার করবেন:

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hello, world!"
```

নমুনা আউটপুট:
```
hello, world!
```

## বিস্তারিত আলোচনা:

ঐতিহাসিকভাবে, প্রোগ্রামিং-এ স্ট্রিং কেসের সাংগঠনিকতা নিশ্চিত করা খুব জরুরি হয়েছে, মূলত প্রথম দিকের কম্পিউটারগুলি খুব কেস-সেনসিটিভ ছিল বলে। Swift এ, `lowercased()` হল স্ট্রিং টাইপের ইন্সট্যান্সে উপলব্ধ একটি মেথড। এটি কল করে, আপনি স্ট্রিংয়ের মধ্যে থাকা সমস্ত অক্ষর যাদের লোয়ারকেস বিকল্প রয়েছে তাদের লোয়ারকেস রূপে রূপান্তরিত করতে পারেন।

`Lowercased()` এর বিকল্প হতে পারে ম্যানুয়ালি স্ট্রিং ট্রাভার্স করা এবং প্রতিটি অক্ষরকে একটি ম্যাপিং ফাংশন ব্যবহার করে তার লোয়ারকেস সমতুল্যে প্রতিস্থাপন করা। কিন্তু, সত্যি বলতে, এটি চাকাটি পুনরায় আবিষ্কার করা।

স্ট্রিং লোয়ারকেসিংয়ে কিছু জটিলতা রয়েছে। উদাহরণস্বরূপ, `lowercased()` মেথড নির্দিষ্ট ভাষার কেসিং নিয়ম সম্পর্কে মোকাবিলা করতে বর্তমান লোকেল ব্যবহার করে, যা সবসময় কাঙ্ক্ষিত আচরণ নাও হতে পারে। আপনি যদি লোকেল-নিরপেক্ষ রূপান্তরগুলি সম্পাদন করতে চান, তাহলে আপনি `lowercased(with: Locale?)` ব্যবহার করতে পারেন এবং লোকেল হিসেবে `nil` পাস করতে পারেন:

```Swift
let turkishString = "İstanbul"
let lowercasedTurkishString = turkishString.lowercased(with: nil)
print(lowercasedTurkishString) // "i̇stanbul", ইউনিকোডে সঠিক, কিন্তু তুরস্কে ডট ছাড়া 'I' আশা করা হতে পারে।
```

`Lowercased()` এর বাস্তবায়ন অধীনে ইউনিকোড মানদণ্ড ব্যবহার করা হয় যা বিভিন্ন স্ক্রিপ্টগুলোর জন্য জটিল ম্যাপিং নিয়ম অন্তর্ভুক্ত করে, যার সব কিছু 'a' দ্বারা 'A' প্রতিস্থাপনের একটি সরল ব্যাপার নয়।

## আরও দেখুন:

Swift এ স্ট্রিং এবং চরিত্র রূপান্তরের উপর আরও অধ্যয়ন করতে, নিম্নলিখিত সম্পদগুলিতে ডুব দিন:

- Swift স্ট্রিং এবং চরিত্র ডকুমেন্টেশন: [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- ইউনিকোড কেসের ম্যাপিং বিস্তারিত: [ইউনিকোড স্ট্যান্ডার্ড](https://www.unicode.org/reports/tr21/tr21-5.html)
- স্ট্রিং তুলনা এবং লোকেলের উপর আলোচনা: [NSHipster নিবন্ধ লোকেলের উপর](https://nshipster.com/locale/)
