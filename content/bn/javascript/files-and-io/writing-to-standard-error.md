---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:48.242304-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Node.js \u098F, stderr \u098F\
  \ \u09B2\u09C7\u0996\u09BE console.error() \u09AE\u09C7\u09A5\u09A1 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF process.stderr \u098F \u09B2\u09BF\u0996\u09C7 \u09B8\
  \u09AE\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09DF\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u0989\u09AD\u09DF \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:44.469666-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u098F, stderr \u098F \u09B2\u09C7\u0996\u09BE console.error() \u09AE\
  \u09C7\u09A5\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0985\u09A5\u09AC\u09BE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF process.stderr \u098F\
  \ \u09B2\u09BF\u0996\u09C7 \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09DF\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0989\u09AD\u09DF\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE\
  \ \u09B9\u09DF\u09C7\u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Node.js এ, stderr এ লেখা console.error() মেথড ব্যবহার করে অথবা সরাসরি process.stderr এ লিখে সম্পাদন করা যায়। এখানে উভয় পদ্ধতি ব্যবহারের উদাহরণ দেওয়া হয়েছে:

```javascript
// console.error() ব্যবহার করে
console.error('এটি একটি এরর মেসেজ।');

// সরাসরি process.stderr এ লেখা
process.stderr.write('এটি আরেকটি এরর মেসেজ।\n');
```

উভয় পদ্ধতির জন্য স্যাম্পল আউটপুট stderr স্ট্রিমে প্রদর্শিত হবে, stdout এর সাথে মিশবে না:
```
এটি একটি এরর মেসেজ।
এটি আরেকটি এরর মেসেজ।
```

আরও উন্নত অথবা অ্যাপ্লিকেশন-নির্দিষ্ট লগিং এর জন্য, অনেক জাভাস্ক্রিপ্ট প্রোগ্রামার `winston` অথবা `bunyan` এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করে থাকেন। `winston` ব্যবহার করে একটি দ্রুত উদাহরণ নিচে দেওয়া হল:

প্রথমে, npm এর মাধ্যমে `winston` ইনস্টল করুন:
```shell
npm install winston
```

তারপর, `winston` কে stderr এ এররগুলি লগ করার জন্য কনফিগার করুন:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// একটি এরর মেসেজ লগ করা
logger.error('winston এর মাধ্যমে লগ করা একটি এরর।');
```

এই সেটআপ নিশ্চিত করে যে, `winston` ব্যবহার করে আপনি যখন একটি এরর লগ করেন, এটি stderr এ নির্দেশ করা হয়, যা স্ট্যান্ডার্ড এবং এরর আউটপুটের মধ্যে স্পষ্ট পৃথকীকরণ বজায় রাখার সাহায্য করে।
