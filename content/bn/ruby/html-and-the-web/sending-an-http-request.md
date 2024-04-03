---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:18.974740-06:00
description: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\
  \u09AC\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09AE\u09CD\u09AA\u09A6 \u09A5\
  \u09C7\u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 API \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0993\
  \u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA\u2026"
lastmod: '2024-03-17T18:47:44.586530-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\
  \u09AC\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09AE\u09CD\u09AA\u09A6 \u09A5\
  \u09C7\u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 API \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0993\
  \u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u0985\u09A5\u09AC\u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\
  \u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09A5\u09BE \u09AC\u09B2\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF\u0964."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
রুবি এটা HTTP অনুরোধ পাঠানো একটা সহজ কাজ করে তোলে। এখানে নেট::এইচটিটিপি স্ট্যান্ডার্ড লাইব্রেরির সাথে দ্রুততম পদ্ধতি দেওয়া হলো।

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

এটি `http://example.com` এর HTML কন্টেন্ট আউটপুট করবে।

আপনি হয়তো ডাটা পোস্ট করতেও চাইবেন:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

এটি ডাটা সহ একটি POST অনুরোধ পাঠায় এবং রেসপন্স দেখায়।

## গভীর ডুব:
অতীতে, HTTP অনুরোধ পাঠানো আরও জটিল ছিল, এবং আপনাকে `HTTParty` এর মতো একটি জেম ব্যবহার করতে হতে পারে। কিন্তু রুবির নিজস্ব `Net::HTTP` লাইব্রেরি অনেক দূর অগ্রসর হয়েছে। এটি এখন আপনার প্রয়োজনের বেশিরভাগ জিনিস সমর্থন করে।

কিন্তু, `Net::HTTP` প্রচুর বাক্যপ্রকার হতে পারে। যদি আপনার প্রজেক্টের আরও HTTP বৈশিষ্ট্য বা সিন্ট্যাক্টিক সুগারের প্রয়োজন হয়, তবে `HTTParty` বা `Faraday` চমৎকার বিকল্প। এই জেমগুলি একটি আরও প্রকাশক এপিআই প্রদান করে এবং মিডলওয়্যার বা ভিন্ন অ্যাডাপ্টারের মতো আরও জটিল পরিস্থিতি সামলাতে পারে।

মৌলিকভাবে, রুবির সাথে একটি HTTP অনুরোধ পাঠানোর মানে হল একটি HTTP ক্লায়েন্ট তৈরি করা, প্রয়োজনে পদ্ধতি, শিরোনাম, এবং বডি সহ একটি অনুরোধ অবজেক্ট সেট আপ করা, তারপর একটি রেসপন্স পেতে অনুরোধটি ডিসপ্যাচ করা।

HTTParty উদাহরণ:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

এটি `Net::HTTP.get` এর সাথে একই কাজ করে কিন্তু কম কনফিগারেশনের সাথে।

## দেখুন এছাড়াও:
আরও বিস্তারিত তথ্যের জন্য, রুবির ডকস খুবই সাহায্যকারী:
- নেট::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

এবং যদি আপনি রুবির HTTP নেটওয়ার্কিং নিয়ে প্রচুর আগ্রহী হন, তাহলে এক নজরে দেখুন:
- রুবির ওপেন ইউআরআই: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- HTTP অনুরোধ টেস্টিংয়ের জন্য ওয়েবমক: https://github.com/bblimke/webmock
