---
title:                "তারিখকে স্ট্রিং এ রূপান্তর করা"
date:                  2024-03-17T17:46:31.740748-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

তারিখকে স্ট্রিংয়ে রূপান্তর করা মানে তারিখ অবজেক্টকে টেক্সট ফরম্যাটে পরিবর্তন করা। প্রোগ্রামাররা এটি পাঠযোগ্যতা, সংরক্ষণ অথবা ব্যবহারকারীদের কাছে তারিখ প্রদর্শনের জন্য করে থাকে।

## কিভাবে:

```TypeScript
// toLocaleString() ব্যবহার করে সহজ রূপান্তর
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // "4/3/2023, 1:15:30 PM" (লোকেল অনুসারে ভিন্ন হবে)

// toISOString() ব্যবহার করে ISO ফরম্যাট
let isoString = date.toISOString();
console.log(isoString); // "2023-04-03T13:15:30.000Z"

// toLocaleDateString() ব্যবহার করে কাস্টম ফরম্যাট
let customString = date.toLocaleDateString('en-US', {
  year: 'numeric',
  month: 'long',
  day: 'numeric',
});
console.log(customString); // "এপ্রিল 3, 2023"
```

## গভীর ডুব

তারিখের স্ট্রিং ফরম্যাটকে তার পাসপোর্ট হিসেবে ভাবুন, যা তাকে সিস্টেম সীমানাগুলির আড়ালে - ডাটাবেস থেকে ওয়েব পৃষ্ঠাগুলিতে যেতে দেয়। ঐতিহাসিকভাবে, আমরা অসমঞ্জস তারিখ ফরম্যাটের সাথে লড়াই করেছি, যার ফলে ISO 8601 এর মতো মানদণ্ড চালু হয়েছে। এটি বিশ্বজুড়ে তারিখ আদান-প্রদান সহজ করে তোলে।

অন্তর্নির্মিত পদ্ধতিগুলির বিকল্প? লাইব্রেরি! বছরের পর বছর ধরে Moment.js ছিল পছন্দের, কিন্তু আজকাল date-fns বা Luxon পছন্দের বিকল্প - তারা হালকা এবং আরো মডুলার।

এই রূপান্তরগুলির মূল পদ্ধতিগুলিতে নিহিত। `toLocaleString()` ব্যবহারকারীর লোকেলের উপর ভিত্তি করে, যা ব্যবহারকারীদের কাছে প্রদর্শনের জন্য আদর্শ। `toISOString()`, অন্যদিকে, ISO 8601 ফরম্যাটে অটল থাকে, যা একটি মানক ফরম্যাটে তারিখ সিরিয়ালাইজ এবং সংরক্ষণ করার জন্য অসামান্য। এবং `toLocaleDateString()` আপনাকে উপস্থিতি নিয়ন্ত্রণ করতে দেয়, নির্দিষ্ট স্টাইলিং প্রয়োজনগুলির প্রতি মনোযোগ দিয়ে।

## আরও দেখুন

- [তারিখ অবজেক্ট - এমডিএন ওয়েব ডকস](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns ডকুমেন্টেশন](https://date-fns.org/docs/Getting-Started)
- [Luxon ডকুমেন্টেশন](https://moment.github.io/luxon/)
- [ISO 8601 তারিখ এবং সময় ফরম্যাট](https://www.iso.org/iso-8601-date-and-time-format.html)
