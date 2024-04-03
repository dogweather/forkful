---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:19.555922-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B6\u09C1\u09B0\u09C1 \u0995\
  \u09B0\u09A4\u09C7, `node-html-parser` \u098F\u09B0 \u09AE\u09A4 \u098F\u0995\u099F\
  \u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0987\u09A8\u09CD\
  \u09B8\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0987 \u099F\u09BE\u09B0\
  \u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u099F\u09BF\
  \ \u09B9\u09B2\u0983."
lastmod: '2024-03-17T18:47:43.762720-06:00'
model: gpt-4-0125-preview
summary: "\u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7, `node-html-parser` \u098F\
  \u09B0 \u09AE\u09A4 \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u0987\u09A8\u09CD\u09B8\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u098F\u0987 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0995\
  \u09AE\u09BE\u09A8\u09CD\u09A1\u099F\u09BF \u09B9\u09B2\u0983."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
শুরু করতে, `node-html-parser` এর মত একটি লাইব্রেরি ইন্সটল করুন। এই টার্মিনাল কমান্ডটি হলঃ

```bash
npm install node-html-parser
```

এখন, চলুন TypeScript এ কিছু বেসিক HTML পার্শিং করি:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

এবং যদি আপনি শুধু কলাগুলো গ্রহণ করতে চান:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## গভীর ডাইভ
HTML পার্শিং নতুন কিছু নয় - এটি ওয়েবের প্রাথমিক দিনগুলি থেকেই চলে আসছে। প্রথমে, ডেভেলপাররা রেগুলার এক্সপ্রেশন ব্যবহার করতে পারে, কিন্তু তা দ্রুত জটিল হয়ে উঠেছিল। তারপর আসে DOM Parser: স্থিতিশীল, কিন্তু ব্রাউজার-বাধ্য।

`node-html-parser` এর মত লাইব্রেরিগুলি ব্যথা দূর করে। এগুলি আপনাকে jQuery এর মত করে HTML কৌড়ি দেয়, কিন্তু Node.js এর সাথে সার্ভার-সাইডে। এটি দ্রুত, নোংরা HTML এর প্রতি সহ্যশীল এবং DOM-বান্ধব।

এছাড়াও আছে `jsdom`, এটি একটি পূর্ণ ব্রাউজার পরিবেশ অনুকরণ করে। এটি ভারী কিন্তু আরো বিস্তৃত, ম্যানিপুলেশন এবং ইন্টারাকশনের জন্য একটি পূর্ণ Document Object Model (DOM) তৈরি করে।

Cheerio কেও ভুলা যায় না। এটি jQuery-এর মত সিনট্যাক্স এবং ছোট্ট ফুটপ্রিন্ট সহ গতি মিশ্রিত করে, দুটোর মাঝামাঝি সুখী ভাবে অবস্থান নেয়।

## আরো দেখুন
আরও জানতে চাইলে, এখানে ডুব দিন:
- [DOM পার্শিং এবং সিরিয়ালাইজেশন W3C স্পেসিফিকেশন](https://www.w3.org/TR/DOM-Parsing/)
- [GitHub এ node-html-parser](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub রিপোজিটরি](https://github.com/jsdom/jsdom)
- [Cheerio ওয়েবসাইট](https://cheerio.js.org/)
