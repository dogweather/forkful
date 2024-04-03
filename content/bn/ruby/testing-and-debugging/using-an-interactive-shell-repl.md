---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:39.998870-06:00
description: "\u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL\
  \ (Read-Eval-Print Loop) \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09AE\u09AF\
  \u09BC \u09AC\u09BE\u0981\u099A\u09BF\u09AF\u09BC\u09C7 \u0995\u09CB\u09A1 \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\
  \ \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.592054-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL\
  \ (Read-Eval-Print Loop) \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09AE\u09AF\
  \u09BC \u09AC\u09BE\u0981\u099A\u09BF\u09AF\u09BC\u09C7 \u0995\u09CB\u09A1 \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\
  \ \u098F\u09AC\u0982 \u09AA\u09C2\u09B0\u09CD\u09A3\u09BE\u0999\u09CD\u0997 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\u09BF \u09A8\
  \u09BE \u0995\u09B0\u09C7 \u09B0\u09C1\u09AC\u09BF\u09B0 \u09AC\u09C8\u09B6\u09BF\
  \u09B7\u09CD\u099F\u09CD\u09AF \u09B6\u09BF\u0996\u09A4\u09C7\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
রুবির REPL কে IRB (Interactive Ruby) বলা হয়। আপনার টার্মিনাল থেকে সরাসরি রুবি চেষ্টা করতে ঝাঁপিয়ে পড়ুন:

```Ruby
irb
2.7.0 :001 > puts "Hello, Ruby world!"
হ্যালো, রুবি বিশ্ব!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
রুবি! রুবি! রুবি! রুবি! রুবি!  => 5
```

## গভীরে ডুব দিন
রুবি 1.8-এ চালু হওয়া IRB রুবিস্টদের জন্য একটি অপরিহার্য সরঞ্জাম। এটি লিস্প এবং পাইথনের ইন্টার‌্যাক্টিভ শেলের অনুপ্রেরণা থেকে তৈরি, যা পরীক্ষামূলক শিক্ষাকে অবিলম্ব প্রতিক্রিয়ার সাথে মিশ্রিত করে। সিনট্যাক্স হাইলাইটিং এবং আরো উন্নত ডিবাগিং পরিবেশের মত আরো বৈশিষ্ট্য সহ বিকল্প যেমন Pry উপস্থিত। IRB নিজেই সাধারণ কিন্তু 'irbtools' এর মত জেমস দিয়ে কার্যকারিতা বাড়ানো যেতে পারে। IRB কিভাবে রিড-এভ্যাল-প্রিন্ট লুপ হ্যান্ডেল করে তা হল প্রতি ইনপুট লাইন পড়া, তাকে রুবি কোড হিসাবে মূল্যায়ন করা, এবং তারপর ফলাফল প্রিন্ট করা, এই প্রক্রিয়াটি বের হওয়া পর্যন্ত চালিয়ে যাওয়া।

## আরও দেখুন
- [রুবির IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [The irbtools gem](https://github.com/janlelis/irbtools)
