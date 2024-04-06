---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:11.922833-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF stderr \u09A4\u09C7 \u09B2\
  \u09BF\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 `$stderr` \u09AC\u09BE `STDERR` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.607864-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF stderr \u09A4\u09C7 \u09B2\u09BF\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 `$stderr` \u09AC\
  \u09BE `STDERR` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964\
  \ \u098F\u0987 \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BE\u09B0 \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09A8\u09C7\u0987\u0964\n"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
রুবির স্ট্যান্ডার্ড লাইব্রেরি stderr তে লিখার জন্য একটি সরাসরি উপায় প্রদান করে `$stderr` বা `STDERR` ব্যবহার করে। এই মৌলিক অপারেশনের জন্য আপনার তৃতীয় পক্ষের লাইব্রেরিগুলির প্রয়োজন নেই।

### stderr এ একটি সাধারণ বার্তা লেখা:
```ruby
$stderr.puts "Error: File not found."
# অথবা সমতুল্য
STDERR.puts "Error: File not found."
```
স্যাম্পল আউটপুট (stderr এ):
```
Error: File not found.
```

### stderr কে একটি ফাইলে রিডাইরেক্ট করা:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Failed to open configuration."
end
```
এই কোড স্নিপেট টি `error.log` নামক একটি ফাইলে stderr রিডাইরেক্ট করে, এবং সকল পরবর্তী ত্রুটি বার্তা তা পর্যন্ত সেখানে আউটপুট হবে যতক্ষণ না প্রোগ্রামটি stderr রিডাইরেকশন রিসেট করে বা শেষ হয়।

### ব্যতিক্রম হ্যান্ডলিংয়ের সাথে stderr ব্যবহার করা:
```ruby
begin
  # এমন একটি অপারেশন সিমুলেট করা যা ব্যর্থ হতে পারে, উদা., একটি ফাইল ওপেন করা
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Exception occurred: #{e.message}"
end
```
স্যাম্পল আউটপুট (stderr এ):
```
Exception occurred: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

রুবির অন্তর্নিহিত পদ্ধতিগুলি অনেক অ্যাপ্লিকেশনের জন্য যথেষ্ট হলেও, আরও জটিল লগিং প্রয়োজনের জন্য, আপনি `logger` স্ট্যান্ডার্ড লাইব্রেরি বা `Log4r` এর মতো বাহ্যিক জেমগুলি বিবেচনা করতে পারেন। এগুলি কনফিগারযোগ্য লগিং পদ্ধতি প্রদান করে, যাতে গুরুত্বের স্তর, ফরম্যাটিং, এবং বিভিন্ন আউটপুটে লেখার সুবিধা অন্তর্ভুক্ত হয়, যেমন ফাইল, ইমেল, এবং আরও অনেক কিছু।
