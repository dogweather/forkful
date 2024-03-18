---
title:                "XML এর সাথে কাজ করা"
date:                  2024-03-17T18:36:04.437553-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
XML-এর সাথে কাজ করা মানে কোড দ্বারা XML (eXtensible Markup Language) নথিগুলিকে পারসিং, জেনারেট এবং ম্যানিপুলেট করা। প্রোগ্রামাররা এটি ওয়েব সার্ভিসেস, কনফিগ ফাইলস এবং ডেটা ইন্টারচেঞ্জ ফরম্যাটের সাথে ইন্টার্যাক্ট করতে করে থাকেন যেখানে XML হল লিঙ্গুয়া ফ্রাঙ্কা।

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
