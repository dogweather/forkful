---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:12.241498-06:00
description: "Fish Shell-\u098F JSON \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BE, \u09AF\u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0 \u0995\
  \u09B0\u09BE, API \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\u09AF\u09BE\u0995\
  \u09B6\u09A8, \u098F\u09AC\u0982 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\
  \u0987\u09A8 \u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09AB\u09CD\u09B2\u09CB\u099C\
  \ \u09B8\u09B9\u099C\u2026"
lastmod: '2024-03-17T18:47:44.519695-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell-\u098F JSON \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BE, \u09AF\u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0 \u0995\
  \u09B0\u09BE, API \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\u09AF\u09BE\u0995\
  \u09B6\u09A8, \u098F\u09AC\u0982 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\
  \u0987\u09A8 \u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09AB\u09CD\u09B2\u09CB\u099C\
  \ \u09B8\u09B9\u099C\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?

Fish Shell-এ JSON নিয়ে কাজ করা মানে JSON ডেটা পার্স করা এবং তৈরি করা, যা অ্যাপ্লিকেশন কনফিগার করা, API ইন্টার‍্যাকশন, এবং কমান্ড-লাইন ওয়ার্কফ্লোজ সহজ করার জন্য একটি সাধারণ কাজ। ওয়েব এবং অ্যাপ্লিকেশন ডেভেলপমেন্টে JSON-এর সর্বব্যাপীতা দেখে, শেলে সরাসরি এর ম্যানিপুলেশনে দক্ষ হওয়া প্রোগ্রামারদের জন্য অটোমেশন এবং ডেটা হ্যান্ডলিং দক্ষতা অনেক বাড়িয়ে দিতে পারে।

## কিভাবে:

Fish Shell নিজে থেকে JSON পার্স করা এবং তৈরি করার জন্য কোন বিল্ট-ইন ইউটিলিটিজ রাখে না। তবে, এটি `jq`-এর মত থার্ড-পার্টি টুলের সাথে নিখুঁতভাবে ইন্টিগ্রেট করে, যা JSON প্রসেসিংয়ের জন্য। `jq` একটি শক্তিশালী ও বহুমুখী কমান্ড-লাইন JSON প্রসেসর যা আপনাকে স্ট্রাকচারড ডেটা কাটা, ফিল্টার করা, ম্যাপ করা, এবং পরিবর্তন করা সম্ভব করে তোলে একটি সহজ এবং প্রকাশমূলক ভাষায়।

### jq দিয়ে JSON পার্স করা
একটি JSON ফাইল পার্স করে ডেটা নেওয়ার জন্য `jq` ব্যবহার করে:

```fish
# ধরুন আপনার কাছে একটি JSON ফাইল আছে যার নাম 'data.json' এবং কন্টেন্ট: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# নমুনা আউটপুট
"Fish Shell"
```

### jq দিয়ে JSON তৈরি করা
শেল ভ্যারিয়েবল বা আউটপুট থেকে JSON কনটেন্ট তৈরি করা:

```fish
# ভ্যারিয়েবল থেকে JSON অবজেক্ট তৈরি করুন
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# নমুনা আউটপুট
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### JSON কালেকশন ফিল্টার করা
ধরুন আমাদের 'versions.json' নামের ফাইলে JSON অবজেক্টের একটি অ্যারে আছে:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
এই অ্যারে থেকে কেবল স্টেবল ভার্সনগুলো ফিল্টার করার জন্য:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# নমুনা আউটপুট
"3.1.2"
"3.4.0"
```

যে উদাহরণগুলো প্রদান করা হয়েছে তা `jq`-এর সাথে Fish Shell ইন্টিগ্রেট করে JSON অপারেশন করার ক্ষমতা দেখায়। এইরূপ টুলগুলো ব্যবহার করে শেল অভিজ্ঞতাকে সমৃদ্ধ করা, আধুনিক ডেটা ফর্ম্যাটগুলো নিয়ে কাজ করার জন্য একটি শক্তিশালী পরিবেশ তৈরি করে।
