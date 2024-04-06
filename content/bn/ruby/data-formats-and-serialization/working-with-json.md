---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:52.255926-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF, \u09A4\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09AE\u09BF\u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7, JSON \u09AA\u09BE\u09B0\
  \u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE\u09B0 \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\
  \u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u0985\u09AA\u09BE\
  \u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u09AE\u09A1\u09BF\u0989\u09B2\
  \ \u09B9\u099A\u09CD\u099B\u09C7 `json`, \u09AF\u09BE \u09AF\u09C7 \u0995\u09CB\u09A8\
  \u2026"
lastmod: '2024-03-17T18:47:44.612979-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF, \u09A4\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AE\u09BF\
  \u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09B8\u09BE\
  \u09A5\u09C7, JSON \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\
  \u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09B8\u09B9\u099C \u0989\
  \u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\
  \u0964 \u098F\u0987 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\
  \u0995 \u09AE\u09A1\u09BF\u0989\u09B2 \u09B9\u099A\u09CD\u099B\u09C7 `json`, \u09AF\
  \u09BE \u09AF\u09C7 \u0995\u09CB\u09A8 \u09B0\u09C1\u09AC\u09BF \u0985\u09CD\u09AF\
  \u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7 \u09B8\u09B9\u099C\u09C7\
  \u0987 \u098F\u0995\u09C0\u09AD\u09C2\u09A4 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\
  \u09BC\u0964\n"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কীভাবে:
রুবি, তার প্রমিত লাইব্রেরির সাথে, JSON পার্স করা এবং তৈরি করার সহজ উপায় প্রদান করে। এই অপারেশনগুলির জন্য প্রাথমিক মডিউল হচ্ছে `json`, যা যে কোন রুবি অ্যাপ্লিকেশনে সহজেই একীভূত করা যায়।

### JSON পার্সিং:
একটি JSON স্ট্রিংকে রুবি হ্যাশে রূপান্তর করতে, আপনি `JSON.parse` মেথড ব্যবহার করতে পারেন।

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# আউটপুট: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSON জেনারেটিং:
অন্যদিকে, একটি রুবি হ্যাশকে JSON স্ট্রিংয়ে রূপান্তর করতে, আপনি `JSON.generate` মেথড অথবা রুবি অবজেক্টের উপর পাওয়া যায় এমন `to_json` মেথড ব্যবহার করতে পারেন একবার `json` লাইব্রেরি দাবি করা হলে।

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# আউটপুট: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### তৃতীয়-পক্ষের লাইব্রেরি:
যদিও রুবির প্রমিত লাইব্রেরি মৌলিক JSON হ্যান্ডলিং কভার করে, অনেক প্রকল্প উন্নত ফাংশনালিটি এবং পার্ফরমেন্সের জন্য তৃতীয়-পক্ষের লাইব্রেরিগুলিতে নির্ভর করে। একটি জনপ্রিয় পছন্দ হল `Oj` (Optimized JSON)।

#### Oj দিয়ে পার্সিং:
```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# আউটপুট: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Oj দিয়ে জেনারেটিং:
Oj আরও একটি দ্রুত উপায় প্রদান করে রুবি অবজেক্ট থেকে JSON তৈরির:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# আউটপুট: {"name":"Samantha","age":35,"city":"Miami"}
```

এই উদাহরণগুলি রুবি দিয়ে JSON নিয়ে কাজ করার সরাসরি পদ্ধতিকে চিত্রিত করে, যা এটিকে সরল ডাটা ম্যানিপুলেশনগুলি থেকে জটিল API যোগাযোগ পর্যন্ত বিভিন্ন কাজের জন্য অ্যাক্সেসিবল করে তোলে।
