---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:21.607472-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `Tempfile` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09B8\u09CD\u09A5\u09BE\
  \u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\u0997 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8 \u09A6\u09C7\u0996\u09BE \u09AF\
  \u09BE\u0995."
lastmod: '2024-03-17T18:47:44.610971-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF `Tempfile` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\
  \u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\u0997 \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8\
  \ \u09A6\u09C7\u0996\u09BE \u09AF\u09BE\u0995."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
রুবির স্ট্যান্ডার্ড লাইব্রেরি `Tempfile` ব্যবহার করে অস্থায়ী ফাইল তৈরি করার সুযোগ প্রদান করে। চলুন দেখা যাক:

```Ruby
require 'tempfile'

Tempfile.create('my_temp') do |tempfile|
  tempfile.write('অস্থায়ী বিষয়বস্তু')
  puts "অস্থায়ী ফাইলটি অবস্থিত আছে: #{tempfile.path}"
end
# ব্লকের পরে, ফাইলটি স্বয়ংক্রিয়ভাবে মুছে ফেলা হয়।
```

এটি চালানোর পরে, আপনি দেখবেন:

```
অস্থায়ী ফাইলটি অবস্থিত আছে: /tmp/my_temp20180418-56789-1234567
```

এই ফাইলটি প্রয়োজন অতিক্রম করার পরে আর থাকবে না। ব্লক শেষ হওয়া মাত্র, রুবি আপনার জন্য পরিষ্কার করে দেয়।

## গভীরভাবে জানুন
`Tempfile` ক্লাসটি রুবি 1.8 থেকে ব্যবহার করা হয়েছে, সময়ের সাথে সাথে এটি অনুশীলন ও পলিশ করা হয়েছে। এটি আপনার সিস্টেমের অস্থায়ী ফাইলের পথ ব্যবহার করে, যা অপারেটিং সিস্টেম দ্বারা প্রদান করা হয়।

বিকল্প? নিশ্চয়ই, আপনি নিজে ম্যানুয়ালি অস্থায়ী ফাইল তৈরি এবং ট্র্যাক করতে পারেন, কিন্তু কেন আগের চাকা আবার আবিষ্কার করবেন? `Tempfile` আপনাকে একটি যাদুঘর, অনন্য ফাইলনাম দেয়, সংঘর্ষের ঝুঁকি হ্রাস করে।

যারা আরও নিয়ন্ত্রণ পাওয়ার জন্য আগ্রহী, `Tempfile.new` পদ্ধতিটি ফাইলের নাম এবং অবস্থান নিয়ে কাজ করার জন্য প্যারামিটার নেয়। কিন্তু মনে রাখবেন, বড় ক্ষমতার সাথে বড় দায়িত্ব আসে - আপনাকে এই ফাইলগুলোকে নিজের দায়িত্বে মুছে ফেলতে হবে।

`Tempfile` ব্যবহারের সত্যিকারের সুবিধা এর সুতা-নিরাপদ এবং জার্বেজ-সংগ্রহকৃত প্রকৃতিতে অবস্থিত। এটি ফাইলটিকে শিকলবন্দী করে এবং নিশ্চিত করে যে গোপনীয় তথ্য তার প্রয়োজনের চেয়ে বেশি সময় ধরে থাকবে না। একটি অস্থায়ী ফাইল একটি মানক ফাইল অবজেক্টের মত আচরণ করে, তাই আপনি এতে পড়তে, লিখতে এবং অন্যান্য সাধারণ ফাইল অপারেশন ব্যবহার করে এটি নিয়ন্ত্রণ করতে পারেন।

## আরও দেখুন
- অস্থায়ী ফাইল ব্যবহারের গভীর উদাহরণের জন্য রুবি API ডক: [API Dock Tempfile](https://apidock.com/ruby/Tempfile)
- ফাইল পরিচালনার উপর রুবি ফাইল I/O গাইড: [File I/O](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
