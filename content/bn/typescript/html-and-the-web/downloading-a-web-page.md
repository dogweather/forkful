---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:52.220052-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\
  \u09A8\u09BF \u09AF\u09C7 URL \u098F \u09AF\u09BE\u09A8 \u09A4\u09BE\u09B0 \u09A5\
  \u09C7\u0995\u09C7 HTML, CSS, \u098F\u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\u09AC\u09A4\
  \ \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09B8\u09AE\u09CD\u09AA\u09A6\
  \ \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09A4\u09C7, \u09A1\u09C7\u099F\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:43.763703-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\
  \u09BF \u09AF\u09C7 URL \u098F \u09AF\u09BE\u09A8 \u09A4\u09BE\u09B0 \u09A5\u09C7\
  \u0995\u09C7 HTML, CSS, \u098F\u09AC\u0982 \u09B8\u09AE\u09CD\u09AD\u09AC\u09A4\
  \ \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09B8\u09AE\u09CD\u09AA\u09A6\
  \ \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09A4\u09C7, \u09A1\u09C7\u099F\u09BE\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09A4\u09C7\
  , \u0986\u09AA\u09A1\u09C7\u099F\u0997\u09C1\u09B2\u09BF \u099A\u09C7\u0995 \u0995\
  \u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\u09B8\
  \u09BE\u0987\u099F\u0997\u09C1\u09B2\u09BF \u0985\u09AB\u09B2\u09BE\u0987\u09A8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0995\u09CD\u09AF\u09BE\u09B6\u09C7 \u0995\u09B0\u09A4\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কি এবং কেন?

ওয়েব পেজ ডাউনলোড করা মানে আপনি যে URL এ যান তার থেকে HTML, CSS, এবং সম্ভবত অন্যান্য সম্পদ নেওয়া। প্রোগ্রামাররা এটি করেন কনটেন্ট প্রক্রিয়া করতে, ডেটা স্ক্র্যাপ করতে, আপডেটগুলি চেক করতে, অথবা ওয়েবসাইটগুলি অফলাইন ব্যবহারের জন্য ক্যাশে করতে।

## কিভাবে:

আপনি TypeScript ব্যবহার করে Node.js এবং `node-fetch` লাইব্রেরির সাহায্যে একটি ওয়েব পেজ ডাউনলোড করতে পারেন। এইভাবে:

```TypeScript
import fetch from 'node-fetch';

async function downloadWebPage(url: string): Promise<void> {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // এটি কনসোলে HTML কন্টেন্ট প্রিন্ট করে
    } catch (error) {
        console.error('Download failed:', error);
    }
}

// ফাংশন ব্যবহার করুন
downloadWebPage('https://example.com');
```

নমুনা আউটপুট (সংক্ষিপ্ত):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## গভীরে যাওয়া

ঐতিহাসিকভাবে, ওয়েব কন্টেন্ট `wget` বা `curl` এর মতো টুলসের মাধ্যমে কমান্ড-লাইন পরিবেশে ডাউনলোড করা হতো। আধুনিক প্রোগ্রামিংয়ে, তবে, আমাদের জাভাস্ক্রিপ্ট/টাইপস্ক্রিপ্ট অ্যাপ্লিকেশনগুলিতে আরও কার্যকারিতা প্রদান করে এমন `node-fetch`, `axios`, বা `request` (অবসৃত কিন্তু এখনও ব্যবহারে) এর মতো লাইব্রেরি রয়েছে।

ওয়েব পেজ ডাউনলোড করার সময়, শুধু HTML নয়। CSS, জাভাস্ক্রিপ্ট, ইমেজ এবং অন্যান্য সম্পদ এর অংশ হতে পারে। সাধারণত, প্রথমে শুধু HTML নেওয়া হয়, এরপর আপনার পেজ থেকে কি প্রয়োজন তার উপর ভিত্তি করে অন্যান্য প্রসেসিং বা ডাউনলোডিং করা হয়।

বাস্তবায়নের দিক দিয়ে, `node-fetch` মূলত Node.js এর জন্য window.fetch API। এটি অনুরোধের প্রতিক্রিয়ার উপর প্রতিশ্রুতির সমাধান করে, আপনাকে একটি টেক্সট স্ট্রিম (.text()), একটি JSON অবজেক্ট (.json()), অথবা এমনকি একটি বাফার (.buffer()) বাইনারি ডেটার জন্য পেতে দেয়।

মনে রাখবেন যে ওয়েব স্ক্র্যাপিং অধিকারগুলি একটি ওয়েবসাইটের `robots.txt` ফাইল এবং সার্ভিসের শর্তাবলী দ্বারা নির্দেশিত হয়। সর্বদা যাচাই করুন যে আপনি একটি সাইট স্ক্র্যাপ করতে অনুমোদিত এবং আইনি সমস্যা বা আপনার IP নিষিদ্ধ হওয়া এড়াতে হারের সীমা সম্মান করুন।

## আরও দেখুন

- [`node-fetch` ডকুমেন্টেশন](https://github.com/node-fetch/node-fetch)
- [Fetch API সম্পর্কে MDN ওয়েব ডকস](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [`axios` লাইব্রেরি](https://github.com/axios/axios)
- [HTTP স্ট্যাটাস কোড (প্রতিক্রিয়াগুলি হ্যান্ডেল করা)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [ওয়েব স্ক্র্যাপিং আইনতত্ত্ব](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)
