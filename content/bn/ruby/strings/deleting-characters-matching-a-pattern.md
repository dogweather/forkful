---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:27.438028-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:44.570827-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
```Ruby
# String#gsub ব্যবহার করে সহজ মুছে ফেলা
example = "Hello, #World!"
cleaned_example = example.gsub(/#/, '') # => "Hello, World!"

puts cleaned_example # আউটপুট: Hello, World!

# একটি চরিত্র সিকোয়েন্স মুছে ফেলা
sequence_example = "Th1s is 2 an example3."
cleaned_sequence = sequence_example.gsub(/[0-9]/, '') # => "This is an example."

puts cleaned_sequence # আউটপুট: This is an example.

# String#delete ব্যবহার করে মুছে ফেলা
delete_example = "Remove vowels from this line."
cleaned_delete = delete_example.delete('aeiou') # => "Rmv vwls frm ths ln."

puts cleaned_delete # আউটপুট: Rmv vwls frm ths ln.
```

## গভীর ডাইভ
ঐতিহাসিকভাবে, রুবি টেক্সট প্রসেসিং-এ জোর দেওয়া একটি ভাষা হিসেবে পরিচিত, যা কিছু দর্শন পার্ল থেকে উত্তরাধিকার সূত্রে পেয়েছে। এর ফলে এটি আপনাকে `gsub` এবং `delete` এর মতো সরঞ্জাম সরাসরি প্রদান করে।

`gsub` মানে গ্লোবাল সাবস্টিটিউশন। এটি প্রায়শই স্ট্রিংয়ের একটি অংশ যা একটি প্যাটার্নের (রেগুলার এক্সপ্রেশন) সঙ্গে মেলে তা অন্য স্ট্রিংয়ের সাথে প্রতিস্থাপন করার জন্য ব্যবহৃত হয়। যখন একটি খালি প্রতিস্থাপন স্ট্রিং প্রদত্ত হয়, এটি কার্যকরভাবে মিলে যাওয়া ক্যারেক্টারগুলোকে মুছে দেয়।

`delete` হল `gsub` এর তুলনায় কম নমনীয় কিন্তু নির্দিষ্ট ক্যারেক্টারগুলো মুছে ফেলা যখন আপনার মূল উদ্দেশ্য হয় তখন এটি দ্রুততর। `delete`-এর সঙ্গে আপনি রেগুলার এক্সপ্রেশন ব্যবহার করতে পারবেন না, তবে সিম্পল ক্যারেক্টার মুছে ফেলার জন্য এটি সোজা পছন্দ।

অবশ্য, এই বিড়ালটির চামড়া ছাড়ানোর অন্যান্য উপায়ও আছে। লাইব্রেরি যেমন `scan` এবং `split` স্ট্রিংগুলিকে বিশ্লেষণ করতে পারে, এবং আপনি তখন তাদের আবার অবাঞ্ছিত ক্যারেক্টারগুলো ছাড়াই পুনর্গঠন করতে পারেন। তবে সরাসরি ক্যারেক্টারগুলো মুছে ফেলার জন্য, `gsub` এবং `delete` হল আপনার সেরা সঙ্গী।

## দেখুন সাথে সাথে
- রুবির `gsub` ডকুমেন্টেশন: [রুবি ডক gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- রুবির `delete` ডকুমেন্টেশন: [রুবি ডক delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- রুবিতে রেগুলার এক্সপ্রেশন: [রুবি Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- রুবির টেক্সট প্রসেসিং দক্ষতা সম্পর্কে গভীর জ্ঞান অর্জনের জন্য "Programming Ruby: The Pragmatic Programmer’s Guide"।
