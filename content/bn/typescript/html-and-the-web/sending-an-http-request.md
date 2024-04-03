---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:17.582377-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F, \u0986\u09AA\
  \u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 Fetch API \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u09AA\u09BE\u09A0\u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\
  , `async/await` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\
  \u09CC\u0995\u09B0\u09CD\u09AF\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.761724-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F, \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3\u09A4 Fetch API \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB, `async/await` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CC\u0995\u09B0\u09CD\u09AF\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
TypeScript-এ, আপনি সাধারণত Fetch API ব্যবহার করে HTTP অনুরোধ পাঠাতে পারেন। এখানে একটি সহজ উদাহরণ দেওয়া হলো, `async/await` ব্যবহার করে সৌকর্যের জন্য:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP ত্রুটি! স্ট্যাটাস: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch ত্রুটি:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

একটি সফল অনুরোধের জন্য নমুনা আউটপুট:

```json
{
  "userId": 1,
  "id": 1,
  "title": "ডিলেকটাস অট অটেম",
  "completed": false
}
```

## গভীর ডাইভ
HTTP অনুরোধ ওয়েবের সূচনা লগ্ন থেকেই অপরিহার্য হয়ে উঠেছে; এটি ব্রাউজার এবং সার্ভারের মধ্যে আলাপ-আলোচনার উপায়। `fetch` আগমনের আগে XMLHttpRequest (XHR) ছিল, যা কাজ করত, কিন্তু তা কাগজের কাজের মতো অনুভব হতো। `fetch`, একটি আধুনিক বিকল্প, promise-based, পরিষ্কার, এবং বেশিরভাগ আধুনিক ব্রাউজারে উইন্ডো অবজেক্টের অংশ।

TypeScript-এ `fetch`-এর বিকল্পের মধ্যে Axios মতো লাইব্রেরিগুলো রয়েছে, যা আরও বৈশিষ্ট্য সরবরাহ করে এবং মাঝে মাঝে ব্যবহারে সহজ হয়। Axios স্বয়ংক্রিয়ভাবে JSON ডেটা পরিবর্তন করে, অনুরোধ বাতিল করার বিকল্প রাখে, এবং উন্নত ত্রুটি ব্যবস্থাপনা প্রদান করে।

পর্দার আড়ালে, TypeScript জাভাস্ক্রিপ্টে কম্পাইল করে। যখন আপনি `fetch` ব্যবহার করে HTTP অনুরোধ পাঠান, আপনি মূলত ব্রাউজারের নেটিভ Fetch API ব্যবহার করছেন। TypeScript-এর টাইপ-চেকিং আপনার কোডের স্থিতিশীলতা বাড়ায় কম্পাইল-সময়ে টাইপ ত্রুটিগুলি ধরে।

## আরও দেখুন
- MDN ওয়েব ডক্স ফেচ সম্পর্কে: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub রেপো: https://github.com/axios/axios
- HTTP অনুরোধ লাইব্রেরিগুলির তুলনা: https://www.npmtrends.com/axios-vs-fetch
