---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:20.251769-06:00
description: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u098F\u0987\u099A\u099F\u09BF\u098F\u09AE\u098F\u09B2\
  \ \u0995\u09CB\u09A1\u09C7\u09B0 \u0997\u09A0\u09A8 \u098F\u09AC\u0982 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u09AC\u09C1\u099D\u09A4\u09C7\
  \ \u098F\u099F\u09BF\u0995\u09C7 \u099B\u09BF\u0981\u09A1\u09BC\u09C7 \u09AB\u09C7\
  \u09B2\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09B7\u09CD\u0995\u09BE\u09B6\
  \u09A8, \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u09AE\u09A3\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09A4\u09A5\u09CD\u09AF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.587871-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u098F\u0987\u099A\u099F\u09BF\u098F\u09AE\u098F\u09B2\
  \ \u0995\u09CB\u09A1\u09C7\u09B0 \u0997\u09A0\u09A8 \u098F\u09AC\u0982 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u09AC\u09C1\u099D\u09A4\u09C7\
  \ \u098F\u099F\u09BF\u0995\u09C7 \u099B\u09BF\u0981\u09A1\u09BC\u09C7 \u09AB\u09C7\
  \u09B2\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09B7\u09CD\u0995\u09BE\u09B6\
  \u09A8, \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u09AE\u09A3\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09A4\u09A5\u09CD\u09AF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F \u0993 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7 \u09AE\u09BE\u0987\u0997\u09CD\u09B0\u09C7\u099F \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8\u0964."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কি এবং কেন?
HTML পার্সিং মানে একটি এইচটিএমএল কোডের গঠন এবং বিষয়বস্তু বুঝতে এটিকে ছিঁড়ে ফেলা। প্রোগ্রামাররা ডেটা নিষ্কাশন, বিষয়বস্তু মণিপুলেট করা অথবা তথ্য বিভিন্ন ফরম্যাট ও সিস্টেমে মাইগ্রেট করার জন্য এটি করে থাকেন।

## কিভাবে:
রুবিতে HTML পার্স করতে, `gem install nokogiri` দিয়ে 'নকোগিরি' জেম ইনস্টল করুন। নকোগিরি এইচটিএমএল এবং এক্সএমএল নিয়ে রুবি ব্যবহারের জন্য একটি সুইস আর্মি ছুরির মতো। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```ruby
require 'nokogiri'
require 'open-uri'

# একটি ওয়েবসাইট থেকে HTML কনটেন্ট লোড করা
html_content = URI.open('http://example.com').read

# HTML পার্স করা
doc = Nokogiri::HTML(html_content)

# শিরোনাম নিষ্কাশন করা
title = doc.xpath('//title').text
puts "পৃষ্ঠার শিরোনাম হল: #{title}"
```

এটি প্রায় এমন কিছু বের করে: `পৃষ্ঠার শিরোনাম হল: Example Domain`.

## গভীর ডুব
রুবির প্রাথমিক দিনগুলিতে, HTML পার্স করার জন্য বিকল্পগুলি সীমিত ছিল। REXML সংযুক্ত ছিল কিন্তু ধীর ছিল। তারপর হ্রিপোটিক প্রকাশিত হয়েছিল, কিন্তু এটি মিলিয়ে গেছে। ২০০৮ সালে নকোগিরি চালু হয়েছিল, হ্রিপোটিকের সহজলভ্যতা এবং প্রমাণিত এক্সএমএল টুলকিট লিবএমএলের গতি এবং শক্তি মিলিয়ে।

পার্সিং জগতে, সবসময় বিকল্প রয়েছে। কিছু লোক 'rexml' লাইব্রেরি বা 'oga', আরেকটি রুবির জন্য XML/HTML পার্সার দ্বারা শপথ করে। কিন্তু নকোগিরি এর দৃঢ়তা এবং গতির জন্য, সেইসাথে এর বিভিন্ন বৈশিষ্ট্যের বিশাল সমারোহের জন্য প্রিয় থেকে গেছে।

আভ্যন্তরীণভাবে, নকোগিরি HTML কে একটি ডকুমেন্ট অবজেক্ট মডেল (DOM)-এ রূপান্তর করে—একটি গাছের গঠন। এটি উপাদানগুলি ন্যাভিগেট এবং মণিপুলেট করা সহজ করে তোলে। XPath এবং CSS সিলেক্টর ব্যবহার করে, আপনি যেকোনো তথ্যের টুকরা চিহ্নিত করতে পারেন।

## আরো দেখুন
- নকোগিরি জেম: [https://nokogiri.org/](https://nokogiri.org/)
- রুবির rexml ডকুমেন্টেশন: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- বিকল্প পার্সার 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- XPath সম্পর্কে জানুন: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
