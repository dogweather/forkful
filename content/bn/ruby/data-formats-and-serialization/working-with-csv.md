---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:49.977908-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u09A1\
  \u09BF\u09AB\u09B2\u09CD\u099F \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 CSV \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE CSV \u09AB\u09BE\u0987\
  \u09B2 \u09A5\u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09A4\
  \u09BE\u09A4\u09C7 \u09B2\u09C7\u0996\u09BE\u0995\u09C7 \u09B8\u09B0\u09B2\u09C0\
  \u0995\u09C3\u09A4 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\
  \u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BF \u098F\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:44.613994-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u09A1\u09BF\u09AB\u09B2\u09CD\u099F \u09B9\u09BF\
  \u09B8\u09C7\u09AC\u09C7 CSV \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\
  \u09C7, \u09AF\u09BE CSV \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AA\
  \u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A4\u09C7 \u09B2\u09C7\u0996\
  \u09BE\u0995\u09C7 \u09B8\u09B0\u09B2\u09C0\u0995\u09C3\u09A4 \u0995\u09B0\u09C7\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
রুবি ডিফল্ট হিসেবে CSV লাইব্রেরি অন্তর্ভুক্ত করে, যা CSV ফাইল থেকে পড়া এবং তাতে লেখাকে সরলীকৃত করে। এখানে কিভাবে সাধারণ কাজের জন্য আপনি এটি ব্যবহার করতে পারেন:

### একটি CSV ফাইল পড়া
CSV ফাইল থেকে পড়তে আপনার প্রথমে CSV লাইব্রেরি প্রয়োজন। তারপর, আপনি সারিগুলি পুনরাবৃত্তি করতে বা তাদের একটি অ্যারেতে পড়তে পারেন।

```ruby
require 'csv'

# প্রতিটি সারি একটি অ্যারে হিসেবে পড়া
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# প্রতি সারির জন্য আউটপুট এরকম দেখাবে: ["data1", "data2", "data3"]
```

### একটি CSV-এ লেখা
একটি CSV ফাইলে লেখাও সরল। আপনি একটি বিদ্যমান ফাইলে অ্যাপেন্ড করতে বা লেখার জন্য একটি নতুন ফাইল তৈরি করতে পারেন।

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# এটি 'output.csv' ফাইল তৈরি করে অথবা নির্দিষ্ট হেডার এবং মানগুলির সাথে উক্ত ফাইলের উপর লিখে।
```

### একটি CSV স্ট্রিং পার্স করা
মাঝে মাঝে আপনার সরাসরি একটি স্ট্রিং থেকে CSV ডাটা পার্স করতে হতে পারে। এখানে কিভাবে:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# প্রত্যাশিত আউটপুট:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### SmarterCSV ব্যবহার করা
আরও জটিল CSV কার্যগুলির জন্য, `SmarterCSV` জেমটি একটি মূল্যবান টুল হতে পারে। প্রথমে, জেমটি ইনস্টল করুন:

```shell
gem install smarter_csv
```

তারপর, আপনি এটি বৃহত ফাইলগুলির সাথে কাজ করা বা আরও জটিল পার্সিং ও ম্যানিপুলেশন সামলানোর জন্য ব্যবহার করতে পারেন:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# এটি 'large_data.csv' পড়বে এবং হেডারগুলির ভিত্তিতে প্রতিটি সারিকে একটি হ্যাশ হিসেবে আউটপুট দেবে।
```

সংক্ষেপে, রুবির বিল্ট-ইন CSV লাইব্রেরি, সাথে `SmarterCSV` এর মতো থার্ড-পার্টি জেমগুলি, CSV ডাটা সামলানোর জন্য দৃঢ় সমর্থন প্রদান করে, যা ডাটা প্রসেসিং ও ম্যানিপুলেশনের কাজের জন্য দক্ষতা নিশ্চিত করে।
