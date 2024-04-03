---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:49.242664-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish-\u098F XML \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\
  \ \u09A8\u09C7\u0987, \u09B8\u09C1\u09A4\u09B0\u09BE\u0982 \u0986\u09AA\u09A8\u09BE\
  \u0995\u09C7 `xmllint` \u09AC\u09BE `xmlstarlet` \u098F\u09B0 \u09AE\u09A4\u09CB\
  \ \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u099F\u09C1\u09B2\u09C7\u09B0 \u0989\
  \u09AA\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09A4\u09C7 \u09B9\
  \u09AC\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.522624-06:00'
model: gpt-4-0125-preview
summary: "Fish-\u098F XML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\
  \u09A8 \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE \u09A8\u09C7\u0987, \u09B8\u09C1\u09A4\
  \u09B0\u09BE\u0982 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 `xmllint` \u09AC\u09BE `xmlstarlet`\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u099F\
  \u09C1\u09B2\u09C7\u09B0 \u0989\u09AA\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09DF\u09BE \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09AE\u09BE\
  \u09A8 \u09AA\u09DC\u09A4\u09C7 \u09B9\u09AF\u09BC."
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
Fish-এ XML পার্সিং এর জন্য কোন বিল্ট-ইন সুবিধা নেই, সুতরাং আপনাকে `xmllint` বা `xmlstarlet` এর মতো বাহ্যিক টুলের উপর নির্ভর করতে হবে। এখানে একটি উদাহরণ দেওয়া হলো কিভাবে মান পড়তে হয়:

```fish
# xmlstarlet দিয়ে XML পার্স করুন
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```
আউটপুট:
```
Hello World
```

XML এডিট করতে এইটা ব্যবহার করুন:

```fish
# xmlstarlet দিয়ে XML এলিমেন্ট এডিট করুন
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

আউটপুট:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## গভীরে যাওয়া:
XML ৯০ এর দশকের শেষের দিকে এসেছে, পাঠ্যযোগ্যতা এবং মেশিন-বান্ধবতা এর জন্য ডিজাইন করা হয়েছে। JSON এর সাদাসিদে ভাষা কিছুটা XML এর জনপ্রিয়তা কমিয়েছে, কিন্তু যেখানে নথি যাচাই এবং নেমস্পেস প্রধান ভূমিকা রাখে, সেখানে XML এখনও প্রতিষ্ঠিত। 

বিকল্প? অবশ্যই—JSON, YAML, অথবা পারফরম্যান্স-প্রেরণা অ্যাপ্লিকেশনের জন্য প্রোটোকল বাফারগুলির মতো বাইনারি ফরম্যাটগুলি। কিন্তু XML এর স্কিমা এবং XSLT (XML পরিবর্তনের জন্য) জটিল পরিস্থিতিতে যেখানে দৃঢ়তা জরুরি, সেখানে বিশেষ গুরুত্ব রাখতে পারে।

অন্তরালে, টুল যেমন `xmlstarlet` শক্তিশালী লাইব্রেরী যেমন libxml2 নিয়ে আসে, যা XPath এবং XQuery দিয়ে আপনাকে সূক্ষ্মভাবে XML খেলা করার সুযোগ দেয়। এগুলো শুধু XML টুল নয় বরং যেকোনো ভাষায় XML স্পর্শ করা DOM ম্যানিপুলেশন এর ধারণাগুলি প্রয়োগ করার গেটওয়ে।

## আরও দেখুন:
- [xmlstarlet ডকুমেন্টেশন](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish ডকুমেন্টেশন](https://fishshell.com/docs/current/index.html)
- [XPath এবং XQuery ফাংশন এবং অপারেটরগুলি](https://www.w3.org/TR/xpath-functions/)
