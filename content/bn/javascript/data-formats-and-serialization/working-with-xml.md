---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:35:56.071817-06:00
description: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\
  \u09AF\u09CD\u09AF\u09C7 XML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\
  \u09BE\u09B0\u09CD\u09B8 (\u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3), \u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F (\u09B8\u09AE\u09CD\
  \u09AA\u09BE\u09A6\u09A8\u09BE) \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 XML\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2,\u2026"
lastmod: '2024-03-17T18:47:44.477701-06:00'
model: gpt-4-0125-preview
summary: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\
  \u09AF\u09CD\u09AF\u09C7 XML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\
  \u09BE\u09B0\u09CD\u09B8 (\u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3), \u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F (\u09B8\u09AE\u09CD\
  \u09AA\u09BE\u09A6\u09A8\u09BE) \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 XML\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2,\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

XML নিয়ে কাজ করা মানে কোডের সাহায্যে XML কন্টেন্ট পার্স (বিশ্লেষণ), ম্যানিপুলেট (সম্পাদনা) এবং তৈরি করা। প্রোগ্রামাররা এটি করে কারণ XML কনফিগারেশন ফাইল, ডেটা এক্সচেঞ্জ এবং ওয়েব সার্ভিসের জন্য ব্যাপকভাবে ব্যবহৃত হয়, এর মানব-পাঠ্য এবং মেশিন-পারসেবল প্রকৃতির কারণে।

## কিভাবে:

এখানে XML পার্স করার উপায় দেখানো হল:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>এই উইকেন্ডে আমাকে ভুলবেন না!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// আউটপুট: User
```

এবং XML তৈরি করতে:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// আউটপুট: <note><to>User</to></note>
```

## গভীর ডাইভ

XML হল eXtensible Markup Language-এর সংক্ষেপ রূপ, যা একটি ডেটা ফরমেট যা ৯০-এর দশক থেকে চালু আছে। এটি এমন এক সেট নিয়ম নির্ধারণ করে যা মানুষ এবং মেশিন উভয়েই পড়তে পারে। ঐতিহাসিকভাবে, XML এর নমনীয়তা এবং কাঠামোবদ্ধ স্তরক্রমের জন্য প্রশংসা পেয়েছে, যা এটিকে SOAP-এর মত ওয়েব সার্ভিস এবং বহু কনফিগারেশন ফাইলের জন্য একটি পছন্দ করে তোলে।

XML-এর বিকল্পের মধ্যে JSON (JavaScript Object Notation) রয়েছে, যা এর JavaScript-এর সাথে সহজ ব্যবহার এবং হালকা ওজনের জন্য জনপ্রিয়। YAML আরেকটি বিকল্প, যা মানুষের পক্ষে বান্ধব এবং কনফিগারেশনের জন্য একটি সাধারণ পছন্দ।

XML-কে JavaScript ব্যবহার করে DOMParser এবং XMLSerializer ইন্টারফেসের মাধ্যমে বাস্তবায়ন করা হয়। XML DOM (Document Object Model) আপনাকে HTML-এর মত করে XML ডকুমেন্ট নেভিগেট এবং সম্পাদনা করতে সহায়তা করে। যদিও JSON-এর উত্থান ঘটেছে, তথাপি বিস্তারিত জানা XML-এর মৌলিক বোঝা গুরুত্বপূর্ণ, কারণ অনেক পুরানো সিস্টেম এবং নির্দিষ্ট শিল্পখাত এখনও ডেটা আদান-প্রদানের জন্য এর উপর নির্ভর করে।

## আরও দেখুন

- MDN ওয়েব ডকস (XML পার্সিং): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM টিউটোরিয়াল): https://www.w3schools.com/xml/dom_intro.asp
- "XML কি?": https://www.w3.org/XML/
