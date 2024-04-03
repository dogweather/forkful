---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:04.092317-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F \u0985\u09B8\
  \u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BE \u09A1\u09BF\u09AB\u09B2\u09CD\u099F\u09AD\u09BE\u09AC\
  \u09C7 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF Node.js-\u098F\
  \u09B0 `fs` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\
  \u09CD\u09AF\u09C7 \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0985\
  \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\
  \u09B0\u09BF \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.785132-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09A1\u09BF\u09AB\
  \u09B2\u09CD\u099F\u09AD\u09BE\u09AC\u09C7 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7\
  \ \u0986\u09AA\u09A8\u09BF Node.js-\u098F\u09B0 `fs` \u09AE\u09A1\u09BF\u0989\u09B2\
  \u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 \u098F\u0987 \u0995\
  \u09BE\u099C\u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\
  \u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u098F\u0995 \u09B8\u09B9\u099C\
  \ \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  \u09CB\u0964."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
TypeScript-এ অস্থায়ী ফাইল তৈরি করা ডিফল্টভাবে নেই, তবে আপনি Node.js-এর `fs` মডিউলের সাহায্যে এই কাজটি করতে পারেন। এখানে অস্থায়ী ফাইল তৈরি এবং ব্যবহারের এক সহজ উপায় দেওয়া হলো।

```typescript
import { mkdtempSync, writeFileSync, readFileSync, unlinkSync } from 'fs';
import { join } from 'path';

// ফাইল ধারণের জন্য একটি অস্থায়ী ডিরেক্টরি তৈরি করুন
const tmpDir = mkdtempSync(join(process.cwd(), 'temp-'));

// অস্থায়ী ফাইল পাথ সংজ্ঞায়িত করুন
const tmpFilePath = join(tmpDir, 'temp-file.txt');

// অস্থায়ী ফাইলে কিছু লিখুন
writeFileSync(tmpFilePath, 'Temporary data');

// ফাইল থেকে ডেটা পুনরায় পড়ুন
const data = readFileSync(tmpFilePath, 'utf-8');
console.log(data); // আউটপুট: Temporary data

// পরিষ্কার: অস্থায়ী ফাইলটি মুছে ফেলুন
unlinkSync(tmpFilePath);
```

এই কোড ব্লকটি একটি অস্থায়ী ফাইল সেটআপ করে, তাতে লিখে, তা থেকে পড়ে এবং তারপর মুছে দিয়ে পরিষ্কার করে।

## গভীর ডুব
অস্থায়ী ফাইলের ধারণা নতুন নয়; তারা প্রোগ্রামিংয়ের আদিকাল থেকে চালু আছে। Unix-এর মতো সিস্টেমগুলিতে অস্থায়ী ফাইলগুলি প্রায়শই `/tmp` বা `/var/tmp`-এ তৈরি করা হয়, এবং Windows `%TEMP%` ব্যবহার করে। আরও সুরক্ষিত বা স্কেলযোগ্য সিস্টেমে, আপনি সাময়িক ডেটা স্টোরেজের জন্য ডাটাবেস বা Redis-এর মতো সার্ভিস ব্যবহার করতে পারেন।

TypeScript-এ, আমরা সাধারণত Node.js-এর `fs` মডিউলের উপর নির্ভর করি, যেমন উপরে দেখানো হয়েছে, তবে আছে `tmp`-এর মতো লাইব্রেরিগুলি যা উন্নত বৈশিষ্ট্য এবং স্বয়ংক্রিয়ভাবে পরিষ্কারের ব্যবস্থা করে। সিস্টেম-নেটিভ অস্থায়ী ডিরেক্টরিগুলি ব্যবহার ঝুঁকিপূর্ণ হতে পারে নামিং সংঘর্ষ বা নিরাপত্তা সমস্যার কারণে। সুতরাং, সংঘর্ষ এবং ফাঁসির এড়িয়ে যাওয়ার জন্য সবসময় ফাইল তৈরি এবং ধ্বংস সাবধানে হ্যান্ডল করুন। তাছাড়া, `uuid`-এর মতো লাইব্রেরিগুলি দ্বারা প্রদত্ত অনন্য নামকরণ সংঘর্ষ এড়াতে পারে। 

শারীরিক টেম্প ফাইলগুলির একটি বিকল্প হ'ল `memfs`-এর মতো মেমোরি-অধিষ্ঠিত ফাইলসিস্টেম ব্যবহার করা। এটি ডিস্ক I/O এড়ায় এবং টেম্প স্টোরেজের প্রয়োজনে অপারেশনগুলি ত্বরান্বিত করতে পারে, তবে এটি সিস্টেম মেমোরি দ্বারা সীমাবদ্ধ।

মনে রাখবেন, অস্থায়ী ফাইল ব্যবহার করার সময়, সংবেদনশীল তথ্যের সাথে সাবধান হন। অস্থায়ী ফাইলগুলি প্রায়শই কম নিরাপদ এবং অন্যান্য প্রক্রিয়া বা ব্যবহারকারীগুলি দ্বারা অ্যাক্সেস করা যেতে পারে, বিশেষ করে ভাগ করা সিস্টেমে।

## আরও দেখুন
- Node.js ফাইল সিস্টেম মডিউল: https://nodejs.org/api/fs.html
- আরও উন্নত টেম্প ফাইল হ্যান্ডলিং-এর জন্য `tmp` লাইব্রেরি: https://www.npmjs.com/package/tmp
- অনন্য নাম জেনারেট করার জন্য `uuid` লাইব্রেরি: https://www.npmjs.com/package/uuid
- মেমোরি-অধিষ্ঠিত ফাইল সিস্টেম লাইব্রেরি `memfs`: https://www.npmjs.com/package/memfs
- অফিসিয়াল TypeScript ডকুমেন্টেশন: https://www.typescriptlang.org/docs/
