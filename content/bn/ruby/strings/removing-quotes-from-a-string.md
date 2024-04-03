---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:51.875959-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09B0\
  \ \u09B8\u09C7\u0987 \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\u0995\u09B0 \u0989\
  \u09A6\u09CD\u09A7\u09B0\u09A3 \u099A\u09BF\u09B9\u09CD\u09A8\u0997\u09C1\u09B2\u09BF\
  \ \u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0995\u09BF\u099B\u09C1 \u09A6\u09BE\u09B0\u09C1\u09A8 \u0995\u09CC\u09B6\
  \u09B2 \u0986\u099B\u09C7\u0964 \u0986\u09AA\u09A8\u09BF `gsub` \u0985\u09A5\u09AC\
  \u09BE `delete` \u09AE\u09C7\u09A5\u09A1\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09AE\u09BE\u09A7\
  \u09BE\u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\u2026"
lastmod: '2024-03-17T18:47:44.574762-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09B8\u09C7\u0987 \u09AC\u09BF\u09B0\u0995\
  \u09CD\u09A4\u09BF\u0995\u09B0 \u0989\u09A6\u09CD\u09A7\u09B0\u09A3 \u099A\u09BF\
  \u09B9\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1 \u09A6\
  \u09BE\u09B0\u09C1\u09A8 \u0995\u09CC\u09B6\u09B2 \u0986\u099B\u09C7\u0964 \u0986\
  \u09AA\u09A8\u09BF `gsub` \u0985\u09A5\u09AC\u09BE `delete` \u09AE\u09C7\u09A5\u09A1\
  \u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\
  \u099C\u099F\u09BF \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\
  \u099B\u09C1 \u0995\u09CB\u09A1 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  \u09CB \u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u099A\u09BF\u09A8\u09CD\u09A4\
  \u09BE \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
রুবির সেই বিরক্তিকর উদ্ধরণ চিহ্নগুলি বাদ দেওয়ার জন্য কিছু দারুন কৌশল আছে। আপনি `gsub` অথবা `delete` মেথডস ব্যবহার করে কাজটি সমাধান করতে পারেন। এখানে কিছু কোড দেওয়া হলো যা নিয়ে চিন্তা করা যায়:

```ruby
# ডাবল এবং সিঙ্গেল উদ্ধৃতি অপসারণের জন্য gsub ব্যবহার করা
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# আউটপুট: Say hello to my little friend!

# যদি আপনি জানেন আপনি শুধুমাত্র এক ধরনের উদ্ধৃতির সাথে যুক্ত হতে যাচ্ছেন
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# আউটপুট: Stay a while and listen!
```

## গভীর ডাইভ
উদ্ধৃতি চিহ্নের ইতিহাস প্রোগ্রামিংয়ের প্রথম সময় পর্যন্ত ফিরে যায়, যেখানে তারা প্রায়শই স্ট্রিং ডিলিমিটারগুলির ভূমিকা পালন করে। আজকের দিনেও, তারা যেমন তখন ছিল, আপনি নিজেকে এই উদ্ধরণ অক্ষরগুলি অপসারণের প্রয়োজনে খুঁজে পেতে পারেন যখন তারা প্রয়োজন হয় না বা যখন তারা ডেটা সংরক্ষণ এবং ম্যানিপুলেশনের সাথে বাধাগ্রস্থ হতে পারে।

আমরা `gsub` এবং `delete` সম্পর্কে আলোচনা করেছি কিন্তু অন্যান্য মেথডগুলিও আছে, যেমন `tr` অথবা `tr_s`, যা আপনাকে একটু বেশি নিয়ন্ত্রণ দেয় অথবা কিছু ভিন্ন ব্যবহারের ক্ষেত্রে সাহায্য করতে পারে:

```ruby
# tr ও উদ্ধৃতি অপসারণে সহায়ক
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# আউটপুট: Do or do not, there is no try.
```

মনে রাখবেন, প্রতিটি মেথডের তার ব্যবহারের ক্ষেত্র আছে। `gsub` আরও শক্তিশালী যখন আপনি জটিল প্যাটার্ন বা একাধিক প্রতিস্থাপনের সাথে মোকাবিলা করছেন। `delete` এবং `tr` সোজা, সরল অক্ষর অপসারণের জন্য সুন্দর কাজ করে।

## আরো দেখুন
অতিরিক্ত পাঠ এবং বৃহত্তর কোডবেসগুলিতে এই মেথডগুলি কাজে লাগানো দেখতে, দেখুন:
- রুবি ডকুমেন্টেশনে [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), এবং [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr)।
- Ruby Monstas এর একটি দারুন [String exercise set](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html) রয়েছে, যা উদ্ধৃতিগুলি নিয়ে কাজ করা অন্তর্ভুক্ত।
- Stack Overflow আলোচনাগুলি [string manipulation](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) বাস্তব বিশ্বের সমস্যা এবং সমাধান সরবরাহ করে, সহকর্মী Rubyists থেকে।
