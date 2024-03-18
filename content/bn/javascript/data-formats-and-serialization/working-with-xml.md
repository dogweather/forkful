---
title:                "XML এর সাথে কাজ করা"
date:                  2024-03-17T18:35:56.071817-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
