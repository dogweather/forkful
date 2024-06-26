---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:35:56.071817-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ XML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.477701-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 XML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\
  \u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09A8\
  \u09CB \u09B9\u09B2."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

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
