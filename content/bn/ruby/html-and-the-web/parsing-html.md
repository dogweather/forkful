---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:20.251769-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09A4\
  \u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\u09C7, `gem install\
  \ nokogiri` \u09A6\u09BF\u09DF\u09C7 '\u09A8\u0995\u09CB\u0997\u09BF\u09B0\u09BF\
  ' \u099C\u09C7\u09AE \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u09A8\u0995\u09CB\u0997\u09BF\u09B0\u09BF \u098F\u0987\u099A\u099F\u09BF\
  \u098F\u09AE\u098F\u09B2 \u098F\u09AC\u0982 \u098F\u0995\u09CD\u09B8\u098F\u09AE\
  \u098F\u09B2 \u09A8\u09BF\u09DF\u09C7 \u09B0\u09C1\u09AC\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\
  \u09BF \u09B8\u09C1\u0987\u09B8\u2026"
lastmod: '2024-03-17T18:47:44.587871-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09A4\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \ \u0995\u09B0\u09A4\u09C7, `gem install nokogiri` \u09A6\u09BF\u09DF\u09C7 '\u09A8\
  \u0995\u09CB\u0997\u09BF\u09B0\u09BF' \u099C\u09C7\u09AE \u0987\u09A8\u09B8\u09CD\
  \u099F\u09B2 \u0995\u09B0\u09C1\u09A8\u0964 \u09A8\u0995\u09CB\u0997\u09BF\u09B0\
  \u09BF \u098F\u0987\u099A\u099F\u09BF\u098F\u09AE\u098F\u09B2 \u098F\u09AC\u0982\
  \ \u098F\u0995\u09CD\u09B8\u098F\u09AE\u098F\u09B2 \u09A8\u09BF\u09DF\u09C7 \u09B0\
  \u09C1\u09AC\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09C1\u0987\u09B8 \u0986\u09B0\
  \u09CD\u09AE\u09BF \u099B\u09C1\u09B0\u09BF\u09B0 \u09AE\u09A4\u09CB\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2\
  ."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

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
