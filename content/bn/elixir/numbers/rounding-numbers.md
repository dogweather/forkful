---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:24.752276-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\u09BE\u0995\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09B8\u09BE\
  \u09A6\u09C3\u09B6\u09CD\u09AF \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\u09A4\u09BE \u09AE\
  \u09C7\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09BE\u099B\u09BE\u0995\u09BE\
  \u099B\u09BF \u09AE\u09BE\u09A8\u09C7 \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\
  \u09CD\u09AF \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09AA\u09A0\u09A8\u09C0\
  \u09AF\u09BC\u09A4\u09BE \u09AC\u09BE\u09DC\u09BE\u09A8\u09CB, \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09A3 \u09B8\u09CD\u09A5\u09BE\u09A8 \u09B9\u09CD\u09B0\u09BE\
  \u09B8 \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.662809-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\u09BE\u0995\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09B8\u09BE\
  \u09A6\u09C3\u09B6\u09CD\u09AF \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\u09A4\u09BE \u09AE\
  \u09C7\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09BE\u099B\u09BE\u0995\u09BE\
  \u099B\u09BF \u09AE\u09BE\u09A8\u09C7 \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\
  \u09CD\u09AF \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09AA\u09A0\u09A8\u09C0\
  \u09AF\u09BC\u09A4\u09BE \u09AC\u09BE\u09DC\u09BE\u09A8\u09CB, \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09A3 \u09B8\u09CD\u09A5\u09BE\u09A8 \u09B9\u09CD\u09B0\u09BE\
  \u09B8 \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
---

{{< edit_this_page >}}

## কী এবং কেন?
সংখ্যা গোলাকার করা মানে এটি সাদৃশ্য বা নির্দিষ্ট নির্ভুলতা মেনে একটি কাছাকাছি মানে সামঞ্জস্য করা। এটি পঠনীয়তা বাড়ানো, সংরক্ষণ স্থান হ্রাস করা অথবা ডোমেইন-বিশেষ প্রয়োজনে, যেমন টাকার হিসেবে নিকটতম সেন্টে গোলাকার করা চাই, উপকারী।

## কীভাবে:
এলিক্সিরে, আপনি `Float.round/2` ব্যবহার করে একটি ভাসমান-পয়েন্ট সংখ্যা গোলাকার করতে পারেন। আপনি রেখে দিতে চাওয়া দশমিক সংখ্যার মান নির্দিষ্ট করতে পারেন। এটি কীভাবে কাজ করে তা এখানে:

```elixir
# কোনো দশমিক স্থান ছাড়া একটি সংখ্যা গোলাকার করুন
Float.round(3.14159) # => 3.0

# ২ দশমিক স্থানে একটি সংখ্যা গোলাকার করুন
Float.round(3.14159, 2) # => 3.14

# নিকটতম ১০ এ একটি নেগেটিভ নির্ভুলতায় সংখ্যা গোলাকার করুন
Float.round(123.456, -1) # => 120.0
```

## গভীর ডুব
সংখ্যা গোলাকার করা কম্পিউটার বিজ্ঞানে একটি ক্লাসিক সমস্যা—এতটাই যে, গোলাকার করার কৌশলের বাছাই আর্থিক সিস্টেম, বৈজ্ঞানিক গণনা, এবং আরো অনেক কিছুতে প্রভাব ফেলতে পারে। এলিক্সিরের `Float.round/2` "হাফ আপ" গোলাকার করার মানদণ্ডে, প্রথাগত গণিত শ্রেণিতে শেখানো গোলাকার করার মত কাজ করে।

অন্যান্য ধরণের গোলাকার করার প্রয়োজন হলে, এলিক্সির আপনাকে নিজের মতো করে তৈরি করার অপশন দেয়। উদাহরণস্বরূপ, "ফ্লোর" গোলাকার করা (সবসময় নিচে) বা "সিলিং" গোলাকার করা (সবসময় উপরে)। যথাক্রমে `Float.floor/1` অথবা `Float.ceil/1` ব্যবহার করবেন।

```elixir
# ফ্লোর গোলাকার করা
Float.floor(3.999) # => 3.0

# সিলিং গোলাকার করা
Float.ceil(3.001) # => 4.0
```

এই বিকল্পগুলি আপনার অ্যাপ্লিকেশনের ঠিক প্রয়োজনের সাথে গোলাকার করার সমন্বয় করতে সাহায্য করে, সেটা আর্থিক হিসাব, গ্রাফিক্স রেন্ডারিং অথবা ডাটা অনুমান হোক না কেন।

## আরও দেখুন
এলিক্সিরের গোলাকার করার ফাংশন এবং ভাসমান-পয়েন্ট সংখ্যাগুলি সম্পর্কে আরও জানতে:

- এলিক্সিরের অফিসিয়াল ডকুমেন্টেশন `Float` এ: https://hexdocs.pm/elixir/Float.html
- ভাসমান-পয়েন্ট অঙ্কনের জন্য IEEE মান (IEEE 754): https://ieeexplore.ieee.org/document/4610935