---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:04.437553-06:00
description: "XML-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1 \u09A6\u09CD\u09AC\u09BE\u09B0\
  \u09BE XML (eXtensible Markup Language) \u09A8\u09A5\u09BF\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u09AA\u09BE\u09B0\u09B8\u09BF\u0982, \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\
  \u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0993\u09DF\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8\u09C7\u09B8,\u2026"
lastmod: '2024-03-17T18:47:44.615908-06:00'
model: gpt-4-0125-preview
summary: "XML-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1 \u09A6\u09CD\u09AC\u09BE\u09B0\
  \u09BE XML (eXtensible Markup Language) \u09A8\u09A5\u09BF\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u09AA\u09BE\u09B0\u09B8\u09BF\u0982, \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\
  \u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0993\u09DF\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8\u09C7\u09B8, \u0995\u09A8\u09AB\
  \u09BF\u0997 \u09AB\u09BE\u0987\u09B2\u09B8 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\
  \u09BE \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u099A\u09C7\u099E\u09CD\u099C \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09A4\
  \u09C7 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09AF\u09C7\u0996\u09BE\
  \u09A8\u09C7 XML \u09B9\u09B2 \u09B2\u09BF\u0999\u09CD\u0997\u09C1\u09AF\u09BC\u09BE\
  \ \u09AB\u09CD\u09B0\u09BE\u0999\u09CD\u0995\u09BE\u0964."
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
