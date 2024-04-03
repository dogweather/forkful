---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:48.377366-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7 \u098F\u0995\u099F\u09BE \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u098F\u099F\u09BE \u099C\u09BE\u09A8\u09BE \u0995\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09B9\u09AC\u09C7 \u09AC\u09BE \u099B\u09BF\u09B2, \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC \u0985\u09A8\u09CD\
  \u09A4\u09B0\u09C7\u09B0 \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u0986\u0997\
  \u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0985\u09A8\u09C1\u09B8\u09CD\u09AE\u09BE\u09B0\u0995, \u09B8\u09BE\u09AC\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09B6\u09A8,\u2026"
lastmod: '2024-03-17T18:47:44.604885-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7 \u098F\u0995\u099F\u09BE \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u098F\u099F\u09BE \u099C\u09BE\u09A8\u09BE \u0995\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09B9\u09AC\u09C7 \u09AC\u09BE \u099B\u09BF\u09B2, \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC \u0985\u09A8\u09CD\
  \u09A4\u09B0\u09C7\u09B0 \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u0986\u0997\
  \u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0985\u09A8\u09C1\u09B8\u09CD\u09AE\u09BE\u09B0\u0995, \u09B8\u09BE\u09AC\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09B6\u09A8, \u0985\u09A5\u09AC\u09BE\
  \ \u0990\u09A4\u09BF\u09B9\u09BE\u09B8\u09BF\u0995 \u09A1\u09C7\u099F\u09BE \u09AC\
  \u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\u09C7\u09B0 \u09AE\u09A4\u09CB \u09AC\
  \u09C8\u09B6\u09BF\u09B7\u09CD\u099F\u09CD\u09AF\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8\u0964."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি & কেন?

ভবিষ্যতে অথবা অতীতে একটা তারিখ নির্ণয় করা মানে এটা জানা কি তারিখ হবে বা ছিল, নির্দিষ্ট সময় অন্তরের পরে অথবা আগে। প্রোগ্রামাররা অনুস্মারক, সাবস্ক্রিপশন, অথবা ঐতিহাসিক ডেটা বিশ্লেষণের মতো বৈশিষ্ট্যগুলির জন্য এটা করে থাকেন।

## কিভাবে:

রুবি তার নির্মিত `Date` ক্লাস এবং কিছু অতিরিক্ত সুবিধার জন্য `active_support` গেম ব্যবহার করে তারিখের সাথে খেলা সহজ করে দেয়। এটি কি ভাবে করা যায় তা নিচে দেখানো হল:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# আজকের তারিখ পান
today = Date.today
puts "আজ হল: #{today}"

# ভবিষ্যতে 10 দিনের তারিখ নির্ণয়
future_date = today + 10
puts "এখন থেকে 10 দিন পরে হবে: #{future_date}"

# অতীতে 30 দিনের তারিখ নির্ণয়
past_date = today - 30
puts "30 দিন আগে ছিল: #{past_date}"

# active_support দ্বারা জটিল হিসাব
puts "2 মাসের মধ্যে, এটা হবে: #{2.months.from_now.to_date}"
puts "100 দিন আগে, এটা ছিল: #{100.days.ago.to_date}"
```

নমুনা আউটপুট:

```
আজ হল: 2023-04-07
এখন থেকে 10 দিন পরে হবে: 2023-04-17
৩০ দিন আগে ছিল: ২০২৩-০৩-০৮
২ মাসের মধ্যে, এটা হবে: ২০২৩-০৬-০৭
১০০ দিন আগে, এটা ছিল: ২০২২-১২-২৮
```

## গভীর ডুব

রুবি তার মান এবং অতিরিক্ত লাইব্রেরিতে তারিখ হিসাবের কার্যকারিতা অন্তর্ভুক্তির আগে, ডেভেলপাররা প্রায়শই ম্যানুয়ালি তারিখ গণনা করতে হতো, অধ্যবসায় বর্ষে, বিভিন্ন মাসের দৈর্ঘ্য এবং সময় অঞ্চল—যা একটা বড় মাথাব্যথার কারণ ছিল।

`Date` ক্লাস স্ট্যান্ডার্ড থেকেই অনেক কিছু করে নেয়। আপনি সহজেই দিন যোগ (+) বা বিয়োগ (-) করতে পারেন। তবে, "এখন থেকে ২ মাস" এর মতো বেশি প্রত্যক্ষ সময় পিরিয়ড ম্যানিপুলেশনের জন্য, আমরা `active_support` এর উপর নির্ভর করি, যা Ruby on Rails থেকে বের করা হয়েছে। এই গেম মান রুবি ক্লাসগুলোর এক্সটেনশন ব্যবহার করে, এমন হিসাবনিকাশকে মানুষের বন্ধুত্বপূর্ণ করে তোলে।

অতীত বা ভবিষ্যত তারিখ হিসাব করার সময়, যদি আপনি সময়ও (`DateTime` বা `Time` অবজেক্টগুলি) মধ্যে আনছেন তবে সময় অঞ্চলগুলিও মনে রাখা উচিত। রুবির `Time` ক্লাস এবং `active_support` এটা সামাল দিতে পারে তবে এটি একটু বেশি সেট আপ প্রয়োজন।

এল্টারনেটিভ হিসেবে, `time-lord` এবং `ice_cube` গেমস রয়েছে, যা যথাক্রমে অধিক সুখপাঠ্য বা বিশেষ বৈশিষ্ট্যগুলির জন্য (যেমন পুনরাবৃত্ত ইভেন্ট) প্রস্তাব করে।

## দেখুন ও

- রুবির সময় অঞ্চল নিয়ে কাজ করা: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- আরও মানুষের মতো প্রকাশের জন্য 'time-lord' গেম: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- পুনরাবৃত্তি ইভেন্ট সামলানোর জন্য 'ice_cube' গেম: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
