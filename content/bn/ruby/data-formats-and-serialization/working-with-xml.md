---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:04.437553-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 Ruby\
  \ \u09B8\u0982\u09AF\u09C1\u0995\u09CD\u09A4 REXML \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09BF \u098F\u0995\u099F\u09BF XML \u09B8\u09CD\u09A8\u09BF\
  \u09AA\u09C7\u099F \u09AA\u09BE\u09B0\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF."
lastmod: '2024-03-17T18:47:44.615908-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 Ruby \u09B8\u0982\u09AF\u09C1\u0995\u09CD\u09A4\
  \ REXML \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF \u098F\u0995\
  \u099F\u09BF XML \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F \u09AA\u09BE\u09B0\u09B8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
চলুন Ruby সংযুক্ত REXML ব্যবহার করি একটি XML স্নিপেট পারস করার জন্য:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') { |element|
  puts "নাম: #{element.attributes['name']}, রঙ: #{element.attributes['color']}"
}
```
আউটপুট:
```
নাম: apple, রঙ: green
নাম: banana, রঙ: yellow
```

XML জেনারেট করাও সহজ:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML আউটপুট:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## গভীর ডুব:
XML-এর মূল সাল ১৯৯০ দশকে ওয়েব নথিগুলির জন্যে SGML এর একটি সরলীকৃত উপসেট হিসেবে ফিরে আসে। এটি বর্ণনামূলক কিন্তু উচ্চমাত্রার কাঠামোগত, এবং এটি কেন এটি সময়ের সাথে সাথে টিকে আছে। এটি শুধুমাত্র খেলার মাঠ নয়-JSON এবং YAML তাদের সরলতার জন্যে জনপ্রিয় হয়ে উঠেছে-কিন্তু XML অনেক এন্টারপ্রাইজ এবং লিগ্যাসি সিস্টেমে শক্ত অবস্থানে আছে।

Ruby XML-এর সাথে মোকাবেলা করার কয়েকটি উপায় প্রদান করে। REXML একটি সম্পূর্ণ-Ruby লাইব্রেরি যা দ্রুত ঝাঁপিয়ে পড়তে সহজ। Nokogiri একটি গেম যা দ্রুত C লাইব্রেরিগুলিকে জড়িত করে, গতি এবং অতিরিক্ত বৈশিষ্ট্য প্রদান করে। তাদের মধ্যে নির্বাচন করা? ছোট কাজের জন্য REXML দিয়ে শুরু করুন এবং আরও শক্তির প্রয়োজন হলে Nokogiriতে স্থানান্তরিত হন।

XML পারসিং এর আড়ালে হল স্ট্রিংগুলিকে DOM অথবা SAX মডেলে অনুবাদ করা। DOM মেমোরিতে একটি গাছ তৈরি করে, অন্যদিকে SAX ডকুমেন্টটিকে স্ট্রিম করে এবং পারস করার সময় ইভেন্টগুলি চালু করে। REXML উভয় মডেল অফার করে, কিন্তু Nokogiri দ্বারা ব্যবহৃত C এক্সটেনশনের চেয়ে ধীর হতে প্রবণ।

## আরও দেখুন:
- Ruby REXML ডকুমেন্টেশন: https://www.rubydoc.info/stdlib/rexml
- Nokogiri গেম: https://nokogiri.org/
- XML স্পেসিফিকেশন: https://www.w3.org/XML/
- SAX পরিচিতি: https://www.saxproject.org/
- YAML বনাম JSON বনাম XML তুলনা: https://www.upwork.com/resources/json-vs-xml-vs-yaml
