---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:32.878665-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 HTML \u0995\u09A8\
  \u09CD\u099F\u09C7\u09A8\u09CD\u099F \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u09A4\u09A5\u09CD\u09AF \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09C7\u09AA \u0995\u09B0\u09BE, \u09AC\u09BE \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0995\u09AD\
  \u09BE\u09AC\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\u0997\u09C1\u09B2\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:44.588973-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 HTML \u0995\u09A8\u09CD\
  \u099F\u09C7\u09A8\u09CD\u099F \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u09A4\u09A5\u09CD\u09AF \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09C7\u09AA \u0995\u09B0\u09BE, \u09AC\u09BE \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0995\u09AD\
  \u09BE\u09AC\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\u0997\u09C1\u09B2\
  \u09BF \u09AA\u09B0\u09CD\u09AF\u09AC\u09C7\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09C7\u09A8\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
`net/http` এবং `open-uri` মতো লাইব্রেরি এবং জেমস দ্বারা Ruby এ ওয়েব পেজ ডাউনলোড করা সহজ। `net/http` ব্যবহার করে এখানে কিভাবে করবেন তা নিচে দেওয়া হল:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com') 
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

আপনি `http://example.com` এর HTML কন্টেন্ট প্রিন্ট করে দেখতে পাবেন।

`open-uri` ব্যবহার করা আরও সহজ:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

আবার, ওয়েব পেজের কন্টেন্ট আপনার টার্মিনালে দেখানো হবে।

## গভীর ডুব
ওয়েবের প্রাথমিক দিনগুলিতে, একটি পৃষ্ঠা ডাউনলোড করা আরও বেশি শ্রম-প্রবণ ছিল, যার মধ্যে ম্যানুয়াল HTTP অনুরোধ তৈরি করা অন্তর্ভুক্ত। আজ, Ruby অনেকটা জটিলতা দূর করে দেয়।

`net/http` এবং `open-uri` এর বিকল্প হিসেবে উচ্চ পর্যায়ের জেমস যেমন `HTTParty` এবং `RestClient` রয়েছে। এগুলি আরও বেশি ফিচার এবং একটি অবজেক্ট-ওরিয়েন্টেড প্রোচেস অফার করে। ভারী দায়িত্বের ওয়েব স্ক্র্যাপিংয়ের জন্য, অনেক Rubyist এরা `Nokogiri` ব্যবহার করে HTML পার্স করার জন্য বা `Mechanize` যা একটি ওয়েব ব্রাউজারের মত কাজ করে।

বাস্তবায়ন সম্পর্কে বিবেচনা করার সময়, মনে রাখবেন `open-uri` হচ্ছে `net/http`-এর জন্য একটি র‍্যাপার, তাই এটি খুব সুবিধাজনক তবে কিছু লো-লেভেল নিয়ন্ত্রণের অভাব থাকতে পারে। `net/http` আপনাকে অনুরোধের উপর আরও নিয়ন্ত্রণ দেয় তবে সাধারণ কাজের জন্য হয়তো বাচ্যবাহুল্য হতে পারে।

## আরও দেখুন
আরও পঠন এবং অতিরিক্ত সম্পদের জন্য, দেখুন:

- Ruby's Net::HTTP ডক: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Open-URI ডক: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Nokogiri's ওয়েবপেজ: [https://nokogiri.org/](https://nokogiri.org/)
- Mechanize জেম রিপোজিটরি: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- HTTParty জেম গিটহাবে: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- RestClient জেম: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
