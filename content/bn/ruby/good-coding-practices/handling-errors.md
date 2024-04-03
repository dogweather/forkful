---
changelog:
- 2024-01-21, dogweather, Reviewed for accuracy
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:57.023463-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF `begin`,\
  \ `rescue`, `ensure`, \u098F\u09AC\u0982 `end` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u098F\u09B0\u09B0 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09B2 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u099D\u09C1\u0981\
  \u0995\u09BF\u09AA\u09C2\u09B0\u09CD\u09A3 \u0995\u09CB\u09A1\u099F\u09BF `begin`\
  \ \u098F\u09AC\u0982 `end` \u098F \u0986\u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09C7\
  \u09A8\u0964 \u09AF\u09A6\u09BF \u0995\u09CB\u09A8\u0993 \u098F\u09B0\u09B0 \u0998\
  \u099F\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.598085-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF `begin`, `rescue`, `ensure`, \u098F\u09AC\u0982\
  \ `end` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u09B0\
  \u09B0 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2 \u0995\u09B0\u09C7\u0964\
  \ \u0986\u09AA\u09A8\u09BF \u099D\u09C1\u0981\u0995\u09BF\u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u0995\u09CB\u09A1\u099F\u09BF `begin` \u098F\u09AC\u0982 `end` \u098F \u0986\
  \u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09C7\u09A8\u0964 \u09AF\u09A6\u09BF \u0995\
  \u09CB\u09A8\u0993 \u098F\u09B0\u09B0 \u0998\u099F\u09C7, `rescue` \u0995\u09BE\u099C\
  \u09C7 \u09B2\u09BE\u0997\u09C7\u0964."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

## কিভাবে:
রুবি `begin`, `rescue`, `ensure`, এবং `end` ব্যবহার করে এরর হ্যান্ডল করে। আপনি ঝুঁকিপূর্ণ কোডটি `begin` এবং `end` এ আবদ্ধ করেন। যদি কোনও এরর ঘটে, `rescue` কাজে লাগে।

```Ruby
begin
  # ঝুঁকিপূর্ণ কোড এখানে যায়।
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "ওহো! আপনি এটা করতে পারবেন না: #{e.message}"
ensure
  puts "এটি সর্বদা চলবে, এরর থাকুক আর না থাকুক।"
end
```

নমুনা আউটপুট:
```
ওহো! আপনি এটা করতে পারবেন না: 0 দিয়ে ভাগ
এটি সর্বদা চলবে, এরর থাকুক আর না থাকুক।
```

## গভীর ডুব
ঐতিহাসিকভাবে, প্রোগ্রামিং ভাষায় এরর হ্যান্ডলিং লক্ষণীয়ভাবে উন্নত হয়েছে, প্রাথমিক ভাষাগুলো প্রায়শই ক্রুড বা অস্তিত্বহীন মেকানিজম আছে। রুবি'র এক্সেপশন হ্যান্ডলিং পাইথন এবং স্মল্টক এর মত ভাষাগুলো দ্বারা অনুপ্রাণিত।

রুবিতে `begin-rescue` এর বিকল্প হল মেথড ডেফিনিশনগুলিতে `rescue` ব্যবহার বা অসামান্য ফ্লো কন্ট্রোলের জন্য `throw` এবং `catch` ব্যবহার করা, যদিও এগুলি সাধারণ এরর হ্যান্ডলিংয়ের জন্য ব্যবহৃত নয়।

একটি আকর্ষণীয় বিষয়: রুবি'র এক্সেপশনগুলি অবজেক্ট (`Exception` ক্লাসের ইন্সট্যান্স এবং এর উত্তরসূরি), তাই আপনি কাস্টম এরর ক্লাসগুলি ডিফাইন করতে পারেন এবং কেবল এররগুলি লগ করার চেয়ে বেশি কিছু করতে পারেন — আপনি আরও শক্তিশালী এরর হ্যান্ডলিংয়ের জন্য প্রোগ্রামে রিচ স্টেট বহন করতে পারেন।

## আরও দেখুন
- এক্সেপশন এবং এরর হ্যান্ডলিং সম্পর্কিত রুবি ডকুমেন্টেশন: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- রুবি এরর হ্যান্ডলিং মূলনীতি সম্পর্কিত বিস্তারিত গাইড: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
