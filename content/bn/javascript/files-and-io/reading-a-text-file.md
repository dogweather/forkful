---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:55.062444-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE\u09B0 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u09A8\u09BF\u09AE\u09CD\u09A8\u09B0\u09C2\u09AA: **Node.js \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 Promises (Async/Await) \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7**."
lastmod: '2024-03-17T18:47:44.470659-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\
  \u09BE\u09B0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09A8\u09BF\u09AE\u09CD\u09A8\
  \u09B0\u09C2\u09AA."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
বর্তমান জাভাস্ক্রিপ্টে একটি টেক্সট ফাইল পড়ার পদ্ধতি নিম্নরূপ:

**Node.js এর সাথে Promises (Async/Await) ব্যবহার করে**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('ফাইল পড়তে গিয়ে একটি ত্রুটি পেয়েছি:', error);
  }
}

readFile('example.txt');
```

নমুনা আউটপুট (‘example.txt’ এর বিষয়বস্তু):

```
Hello, this is a text file!
```

**ব্রাউজারে fetch API ব্যবহার করে**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('ওহো, ফাইল ফেচ করতে গিয়ে কিছু ভুল হয়েছে:', error);
  }
}

fetchTextFile('example.txt');
```

## গভীরে ডুব:
মূলত, জাভাস্ক্রিপ্টে ফাইল পড়া বেশিরভাগ সার্ভার-সাইডের বিষয় ছিল, যা Node.js এর দ্বারা সামলানো হতো। JS HTML5 এর সাথে ব্রাউজারে নিয়ে এলে, `FileReader` এবং `fetch` এর মতো APIs এসেছে, যা ক্লায়েন্ট-সাইড ফাইল পঠনকে সহজ করে তোলে।

বিকল্প? হ্যাঁ, কয়েকটি আছে। স্ট্রিম বড় ফাইল নিয়ে চলতে পারে মেমরি হোগিং ছাড়াই। ওয়ার্কারস UI ফ্রিজ আপকে প্রতিরোধ করে। লাইব্রেরিগুলি জটিল কাজগুলিকে সহজ করে দেয়। প্রত্যেকের নিজস্ব স্থান আছে।

অভ্যন্তরীণভাবে, ফাইল পড়া হয়তো বাফার ম্যানেজমেন্ট, ক্যারেক্টার এনকোডিং (UTF-8, ইত্যাদি), এবং ত্রুটি হ্যান্ডলিং জড়িত থাকতে পারে। নিরাপত্তার বিষয়টিও মাথায় রাখুন; ভালো কারণেই ব্রাউজারগুলি ফাইল অ্যাক্সেসে সীমাবদ্ধতা আরোপ করে।

## দেখুন এছাড়াও
এই সম্পদগুলির সাথে আপনার শেখার প্রক্রিয়াকে আরো এগিয়ে নিন:

- MDN's FileReader API ডকুমেন্টেশন: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Node.js ফাইল সিস্টেম ডকস: [Node.js fs](https://nodejs.org/api/fs.html)
- বড় ফাইলের জন্য Stream API: [Node.js stream](https://nodejs.org/api/stream.html)
- fetch API বুঝতে: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
