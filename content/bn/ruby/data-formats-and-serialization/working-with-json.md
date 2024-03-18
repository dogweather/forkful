---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:52.255926-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

JSON (JavaScript Object Notation) হল একটি হালকা ডাটা আদানপ্রদানের ফর্ম্যাট, যা ওয়েব অ্যাপ্লিকেশনগুলিতে ক্লায়েন্ট এবং সার্ভারের মধ্যে ডাটা আদানপ্রদানের জন্য ব্যাপক প্রচলিত। প্রোগ্রামাররা রুবি দিয়ে JSON নিয়ে কাজ করে থাকেন বাইরের সোর্স থেকে পাওয়া ডাটাকে পার্স করতে অথবা API রেসপন্সের জন্য ডাটাকে ফর্ম্যাট করতে, তার মানব-পাঠ্য স্ট্রাকচারকে কাজে লাগিয়ে সহজেই ডাটা ম্যানিপুলেশন এবং স্টোরেজের জন্য।

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
