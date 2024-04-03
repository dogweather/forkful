---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:10.474195-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:43.790224-06:00'
model: gpt-4-0125-preview
summary: .
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
```TypeScript
import { parseString } from 'xml2js';

// নমুনা XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>মিটিংটি ভুলে যাবেন না!</body>
             </note>`;

// XML কে JSON এ পার্স করা
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// ধরা যাক, পার্সিং সফল হয়েছিল, আউটপুট দেখতে হতে পারে:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['মিটিংটি ভুলে যাবেন না!'] } 
}
```

## গভীর ডুব
XML, অর্থাৎ Extensible Markup Language, '90 এর দশকের শেষে চালু হয়েছিল। এর স্ব-বর্ণনামূলক স্বাভাবিকতা এবং মানব-পাঠ্য ফরম্যাট RSS feeds, কনফিগুরেশন ম্যানেজমেন্ট এবং Microsoft Office Open XML এর মত অফিস ডকুমেন্ট ফরম্যাটগুলির জন্য এটিকে শুরুতেই একটি হিট করে তোলে। কিন্তু, JSON এর তুলনায় এটি বাচকশব্দগত। JSON ওয়েব ভিত্তিক APIs এর জন্য এর হালকা ওজন এবং জাভাস্ক্রিপ্টের সাথে স্বাভাবিক সামঞ্জস্যের জন্য আলোচনায় এসেছে।

যাইহোক, XML মৃত নয়। এটি বড় মাপের এন্টারপ্রাইজ সিস্টেমগুলিতে ব্যবহৃত হয় এবং সেই ডকুমেন্ট স্ট্যান্ডার্ডগুলির জন্য যেগুলি JSON এ পরিবর্তন হয়নি। `xml2js` টাইপস্ক্রিপ্টের জন্য বা পাইথনে `lxml` এর মত টুলস প্রমাণ করে যে প্রোগ্রামিংয়ে XML এর ম্যানিপুলেশনের এখনও প্রয়োজন আছে।

TypeScript এ JSON এর মত সরাসরি XML এর জন্য সমর্থন নেই। বরং, আপনাকে লাইব্রেরিগুলির সাথে কাজ করতে হয়। `xml2js` একটি উদাহরণ। এটি XML কে JSON এ রূপান্তর করে, যার ফলে ডেটা জাভাস্ক্রিপ্টের গুরুদের কাজ করে সহজ হয়ে যায়।

## আরও দেখুন
- [MDN Web Docs এ XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm package](https://www.npmjs.com/package/xml2js)
- [W3Schools XML টিউটোরিয়াল](https://www.w3schools.com/xml/)
