---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:49.242664-06:00
description: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09BE \u09AC\u09CD\u09AF\u09BE\
  \u09AA\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\
  , \u0997\u09A0\u09BF\u09A4 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A5\
  \u09BE\u0995\u09C7 \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8, \u09AE\u09C7\u09B8\u09C7\u099C\u09BF\u0982 \u098F\u09AC\u0982 \u0986\
  \u09B0\u09CB \u0985\u09A8\u09C7\u0995 \u0995\u09BF\u099B\u09C1\u09A4\u09C7 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964\u2026"
lastmod: '2024-03-17T18:47:44.522624-06:00'
model: gpt-4-0125-preview
summary: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09BE \u09AC\u09CD\u09AF\u09BE\
  \u09AA\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\
  , \u0997\u09A0\u09BF\u09A4 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A5\
  \u09BE\u0995\u09C7 \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8, \u09AE\u09C7\u09B8\u09C7\u099C\u09BF\u0982 \u098F\u09AC\u0982 \u0986\
  \u09B0\u09CB \u0985\u09A8\u09C7\u0995 \u0995\u09BF\u099B\u09C1\u09A4\u09C7 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964\u2026"
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

XML এর সাথে কাজ করা মানে ডেটা নিয়ে কাজ করা যা ব্যাপকভাবে ব্যবহৃত, গঠিত ফরম্যাটে থাকে যা কনফিগারেশন, মেসেজিং এবং আরো অনেক কিছুতে ব্যবহৃত হয়। প্রোগ্রামাররা XML নিয়ে কাজ করে, ডেটা পড়তে, লিখতে, আপডেট এবং জিজ্ঞাসা করে—যা অনেক অ্যাপ এবং সেবায় ইন্টারঅপারেবিলিটির জন্য জরুরি।

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
