---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:44.539315-06:00
description: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8\u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u09AF\u09CB\u0997\
  \u09BE\u09AF\u09CB\u0997 \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u0989\
  \u09AA\u09BE\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE \u0986\
  \u09A6\u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8, \u09B8\u09AE\u09CD\u09AA\
  \u09A6 \u0986\u09A8\u09A4\u09C7 \u09AC\u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\
  \u09B0\u09C7 \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.449658-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8\u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u09AF\u09CB\u0997\
  \u09BE\u09AF\u09CB\u0997 \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u0989\
  \u09AA\u09BE\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE \u0986\
  \u09A6\u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8, \u09B8\u09AE\u09CD\u09AA\
  \u09A6 \u0986\u09A8\u09A4\u09C7 \u09AC\u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\
  \u09B0\u09C7 \u09A1\u09C7\u099F\u09BE\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি HTTP অনুরোধ পাঠানো হচ্ছে আপনার জাভাস্ক্রিপ্ট কোডের সাথে একটি সার্ভারের যোগাযোগ স্থাপনের উপায়। এটি ডেটা আদান-প্রদান, সম্পদ আনতে বা সার্ভারে ডেটা প্রক্রিয়া করার জন্য ইত্যাদি করা হয়।

## কিভাবে:

জাভাস্ক্রিপ্ট অনুরোধ পাঠানোর জন্য `fetch` API ব্যবহার করে। এখানে একটি সিম্পল GET অনুরোধ কিভাবে করতে হয় তা দেখানো হল:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('ত্রুটি:', err));
```

আউটপুট হবে URL থেকে JSON ডেটা। সহজ, তাই না?

এবং একটি POST অনুরোধের জন্য:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('ত্রুটি:', err));
```

এটি নতুন ডেটা পাঠায় এবং সার্ভারের প্রতিসাদ দেখায়।

## গভীর ডুব

HTTP অনুরোধ ওয়েবের শুরু থেকেই আছে—HTML ফর্মগুলি চিন্তা করুন। XMLHttpRequest (XHR) একসময় জাভাস্ক্রিপ্টে অনুরোধ পাঠানোর জন্য যাওয়া-আসা পদ্ধতি ছিল, কিন্তু এটি জটিল।

`fetch` এর প্রবেশ, একটি আধুনিক পদ্ধতি যা প্রমিস-ভিত্তিক, এবং পরিষ্কার ও আরো মজবুত করে তোলে। XHR এর বিপরীতে, `fetch` একটি একক, একীভূত API এ উভয় অনুরোধ এবং প্রতিক্রিয়াগুলিকে সামলায় এবং ভাষার মধ্যে নির্মিত, কোন লাইব্রেরির প্রয়োজন নেই।

বিকল্প? অবশ্যই। Axios বা jQuery's Ajax মতো লাইব্রেরিগুলি এখনও ব্যবহার করা হয়। তারা কিছু সিনট্যাকটিক মিষ্টি এবং নির্দিষ্ট বৈশিষ্ট্যের জন্য কাজের উপায় প্রদান করে, কিন্তু `fetch` হল নেটিভ এবং সাধারণত এগিয়ে যাওয়ার পথ।

বাস্তবায়ন বিস্তারিত? ত্রুটি সামলানো, বিভিন্ন প্রতিক্রিয়া প্রকার নিয়ে কাজ করা এবং ক্রস-অরিজিন রিসোর্স শেয়ারিং (CORS) নিয়মাবলীর সম্পর্কে সচেতন থাকা মনে রাখবেন।

## আরও দেখুন

- MDN `fetch` API ডকুমেন্টেশন: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- জাভাস্ক্রিপ্টে প্রমিস ব্যবহার করা: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- CORS সম্পর্কে জানুন: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
