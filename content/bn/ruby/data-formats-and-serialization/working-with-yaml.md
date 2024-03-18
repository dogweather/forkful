---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:45.049530-06:00
description: "YAML \u09AF\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 YAML Ain't Markup Language,\
  \ \u098F\u099F\u09BF \u09A4\u09BE\u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\
  \u09CD\u09AF \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u0995\u09BE\
  \u09B0\u09A3\u09C7 Ruby-\u09A4\u09C7 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE\
  \ \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09BE\u09AA\u0995\
  \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.611977-06:00'
model: gpt-4-0125-preview
summary: "YAML \u09AF\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 YAML Ain't Markup Language,\
  \ \u098F\u099F\u09BF \u09A4\u09BE\u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\
  \u09CD\u09AF \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u0995\u09BE\
  \u09B0\u09A3\u09C7 Ruby-\u09A4\u09C7 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE\
  \ \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09BE\u09AA\u0995\
  \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
YAML যার অর্থ YAML Ain't Markup Language, এটি তার মানব-পাঠ্য ফরম্যাটের কারণে Ruby-তে কনফিগারেশন ফাইল এবং ডেটা সিরিয়ালাইজেশনের জন্য ব্যাপকভাবে ব্যবহৃত হয়। প্রোগ্রামাররা YAML এর দিকে ঝুকে থাকেন যখন তাদের ডেটা অবজেক্টগুলি একটি পাঠ্যযোগ্য তবে গঠনমূলক উপায়ে সংরক্ষণ বা সম্প্রেষণের প্রয়োজন হয়, যা কনফিগারেশন ম্যানেজমেন্ট, ডেটা স্টোরেজ, এবং বিভিন্ন ভাষার মধ্যে ডেটা শেয়ারিং এর মতো কাজগুলি সরল করে তোলে।

## কিভাবে:
Ruby একটি বিল্ট-ইন লাইব্রেরি সাথে আসে যাকে বলা হয় Psych, যা YAML পার্সিং এবং এমিটিংয়ের জন্য ব্যবহৃত হয়। এটি ব্যবহার করতে, আপনাকে প্রথমে YAML স্ট্যান্ডার্ড লাইব্রেরি অনুরোধ করা প্রয়োজন। এখানে একটি বেসিক উদাহরণ দেওয়া হলো যা আপনাকে শুরু করতে সাহায্য করবে:

```ruby
require 'yaml'

# সিরিয়ালাইজ হওয়া হ্যাশ
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# হ্যাশটিকে YAML এ রূপান্তরিত করা
yaml_data = person.to_yaml

puts yaml_data
```

**সাম্প্রতিক আউটপুট:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Ruby অবজেক্টে YAML ডেটা ফিরে লোড করার জন্য:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**সাম্প্রতিক আউটপুট:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করা:

যদিও স্ট্যান্ডার্ড লাইব্রেরি মৌলিক কাজের জন্য যথেষ্ট, জটিল চাহিদার জন্য আপনি 'safe_yaml' এর মতো থার্ড-পার্টি গেমগুলিতে চোখ রাখতে পারেন। এমন লাইব্রেরিগুলি ব্যবহার করতে, আপনাকে প্রথমে গেম ইনস্টল করতে হবে:

```bash
gem install safe_yaml
```

তারপর, আপনি এটি ব্যবহার করে YAML ডেটা নিরাপদে লোড করতে পারেন, যেমন ব্যবহারকারী-নিয়ন্ত্রিত উত্�্র থেকে অবজেক্ট অ্যানস্ট্যান্টিয়েশনের মতো ঝুঁকি হ্রাস করা:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**সাম্প্রতিক আউটপুট:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

এই পদ্ধতি আপনার YAML হ্যান্ডলিং-এর নিরাপত্তা বৃদ্ধি করে, যা অবিশ্বাস্য উত্�্র থেকে YAML লোড করা অ্যাপ্লিকেশনের জন্য একটি ভাল পছন্দ হতে পারে।
