---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:34.691116-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09BE\
  \u09B0\u09CD\u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09AA\u09C7\u099C\u099F\
  \u09BF \u0997\u09A0\u09A8 \u0995\u09B0\u09BE HTML, CSS, JavaScript, \u098F\u09AC\
  \u0982 \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09A1\u09C7\u099F\u09BE\
  \ \u09B8\u0982\u0997\u09CD\u09B0\u09B9 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE \u0995\
  \u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.451971-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09AA\u09C7\u099C\u099F\u09BF\
  \ \u0997\u09A0\u09A8 \u0995\u09B0\u09BE HTML, CSS, JavaScript, \u098F\u09AC\u0982\
  \ \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09A1\u09C7\u099F\u09BE \u09B8\
  \u0982\u0997\u09CD\u09B0\u09B9 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE \u0995\u09A8\
  \u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  , \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09CD\u09AF\u09BE\u0995\u09B6\u09A8 \u0985\
  \u099F\u09CB\u09AE\u09C7\u099F \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\u09BE \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u0986\u09B0\u09CD\u0995\u09BE\u0987\
  \u09AD \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09C7\u09A8\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কি এবং কেন?

ওয়েব পেজ ডাউনলোড করা মানে সার্ভার থেকে পেজটি গঠন করা HTML, CSS, JavaScript, এবং অন্যান্য ডেটা সংগ্রহ করা। প্রোগ্রামাররা তা কনটেন্ট পার্স করা, ইন্টার্যাকশন অটোমেট করা, অথবা ওয়েব পেজ আর্কাইভ করার জন্য করে থাকেন।

## কিভাবে:

নোড.জেএস দিয়ে `node-fetch` ব্যবহার করে একটি পেজ ডাউনলোড করার একটি দ্রুত উপায় এখানে দেওয়া হল:

```Javascript
const fetch = require('node-fetch'); // প্রথমে এটি ইনস্টল করতে হতে পারে!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // পেজের HTML সোর্স আউটপুট দেয়
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

নমুনা আউটপুট:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## গভীর ডুব

ঐতিহাসিকভাবে, ওয়েব পেজ ডাউনলোড করা হতো ব্রাউজারে XMLHTTPRequest অথবা নোড.জেএসে `http` মডিউলের মাধ্যমে। তবে, ES6-এর পর, `fetch` API এর সহজ সিনট্যাক্স এবং Promise-ভিত্তিক প্রকৃতির কারণে আধুনিক মানদণ্ড হয়ে উঠেছে।

বিকল্পগুলির মধ্যে `axios` রয়েছে, যা নেটিভ fetch এর তুলনায় একটু বেশি কার্যকারিতা সহ রিকোয়েস্ট সামলায়। জটিল ব্যবহারের ক্ষেত্রে, জাভাস্ক্রিপ্ট-রেন্ডার কনটেন্ট নিয়ে কাজ করার জন্য হেডলেস ব্রাউজারে পেজটি প্রকৃতপক্ষে রেন্ডার করতে আপনি `puppeteer` ব্যবহার করতে পারেন।

পেজ ডাউনলোড বাস্তবায়নের সময় `robots.txt` মেনে চলা, `User-Agent` সামলানো যাতে ব্লক না হয়ে যায়, এবং সার্ভার ওভারলোড অথবা রেস কন্ডিশনের সাথে সম্ভাব্য সমস্যা এড়াতে অ্যাসিঙ্ক্রোনাস হ্যান্ডলিংকে যত্নসহকারে পরিচালনা করার মতো দিকগুলি লক্ষ্য রাখা উচিত।

## আরও দেখুন

- `fetch` API সম্পর্কে MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Axios GitHub পৃষ্ঠা: https://github.com/axios/axios
- Puppeteer GitHub পৃষ্ঠা: https://github.com/puppeteer/puppeteer
- ওয়েব স্ক্র্যাপিং শ্রেষ্ঠ প্রক্রিয়াগুলি সম্পর্কে একটি নিবন্ধ: https://www.scrapingbee.com/blog/web-scraping-best-practices/
