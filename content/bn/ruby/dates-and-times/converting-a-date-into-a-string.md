---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:53.733539-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09A8\u09BF\u09DF\u09C7 \u0996\u09C7\u09B2\u09BE \u0995\u09B0\u09BE \u09B8\
  \u09C1\u09AA\u09BE\u09B0 \u09B8\u09B9\u099C \u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09A6\u09C7\u0996\u09C1\u09A8 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\
  \u09AC\u09C7 \u098F\u099F\u09BE \u0995\u09B0\u09AC\u09C7\u09A8."
lastmod: '2024-03-17T18:47:44.602572-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A8\u09BF\u09DF\u09C7 \u0996\u09C7\
  \u09B2\u09BE \u0995\u09B0\u09BE \u09B8\u09C1\u09AA\u09BE\u09B0 \u09B8\u09B9\u099C\
  \ \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09C1\u09A8 \u0986\u09AA\
  \u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BE \u0995\u09B0\
  \u09AC\u09C7\u09A8."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
রুবি তারিখ এবং স্ট্রিং নিয়ে খেলা করা সুপার সহজ । এখানে দেখুন আপনি কিভাবে এটা করবেন:

```Ruby
require 'date'

# চলুন একটি তারিখ অবজেক্ট তৈরি করি
my_date = Date.new(2023, 4, 14)

# ডিফল্ট স্ট্রিংয়ে রূপান্তর
date_string = my_date.to_s
puts date_string  # আউটপুট: "2023-04-14"

# strftime (string format time) ব্যবহার করে স্বনির্ধারিত ফর্ম্যাট
সুন্দর_তারিখ = my_date.strftime('%B %d, %Y')
puts সুন্দর_তারিখ  # আউটপুট: "এপ্রিল 14, 2023"

# আরেকটি উদাহরণ, শুধু মজার জন্য
মজাদার_তারিখ_ফর্ম্যাট = my_date.strftime('%d-%m-%Y')
puts মজাদার_তারিখ_ফর্ম্যাট  # আউটপুট: "14-04-2023"
```

## গভীর ডুব
অনেক আগে, মানুষ হাতে তারিখ লিখত। প্রোগ্রামিং বিশ্বে, রুবির `Date` ক্লাস আমাদের ঘাম ছাড়াই তারিখ নিয়ে কাজ করার ক্ষমতা দিয়েছে। আপনার `Date` অবজেক্টগুলোকে স্ট্রিংয়ে পরিণত করার জন্য `to_s` এবং `strftime` মেথডের মত উপাদান রয়েছে।

`to_s` মেথড আপনাকে একটি দ্রুত ISO 8601 প্রতিনিধিত্ব (`YYYY-MM-DD`) দেয়, যা কোনো ফ্রিলস ছাড়া রূপান্তরের জন্য দারুন। কিন্তু যখন আপনি চান আপনার তারিখ একটি ফ্যান্সি পোশাক পরুক, `strftime` আপনাকে আপনার স্ট্রিংটি কোন নির্দিষ্ট প্যাটার্ন অনুসরণ করবে তা চয়ন করার অনুমতি দেয়। `strftime` এর মধ্যে `%Y` চার অঙ্কের বছরের জন্য, `%m` দুই অঙ্কের মাসের জন্য, এবং `%d` দুই অঙ্কের দিনের জন্য প্রতীক গুলো আপনার তারিখ ফর্ম্যাট করার বেসিক বিল্ডিং ব্লক।

`Timecop` এর মত জেম যা পরীক্ষা চলাকালীন সময় ভ্রমণ (বাস্তব সময় ভ্রমণ নয়, দুঃখিত) জন্য, অথবা `Chronic` যা প্রাকৃতিক ভাষার তারিখ পার্স করে, যখন আপনার প্রয়োজন হবে তখন এটি কিছু অতিরিক্ত শক্তি যোগ করতে পারে।

মূল কথা? রুবি C লাইব্রেরীগুলির মতো সিস্টেম লাইব্রেরীগুলি—অধীনে ব্যবহার করে। এর মানে এটা দ্রুত এবং ব Reliableিশ্বস্ত, অধিবর্ষ এবং দিনের আলো সঞ্চয় সময়ের মত বিষয়গুলো সামলাতে একজন চ্যাম্পিয়নের মত।

## আরও দেখুন
আরও বিস্তারিতের জন্য এই সম্পদগুলো পরীক্ষা করুন:
- রুবির `Date` ক্লাস ডকুমেন্টেশন: [ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- রুবির `strftime` নির্দেশিকা: [apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
- তারিখ/সময় জাদুর জন্য আরও জেমস: [github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop) এবং [github.com/mojombo/chronic](https://github.com/mojombo/chronic)
