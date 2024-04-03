---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:44:05.528939-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript, \u098F\u0995\u099F\
  \u09BF JavaScript-\u098F\u09B0 \u09B8\u09C1\u09AA\u09BE\u09B0\u09B8\u09C7\u099F\
  \ \u09B9\u0993\u09AF\u09BC\u09BE\u09AF\u09BC, stderr \u09B2\u09C7\u0996\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF JS \u09B0\u09BE\u09A8\
  \u099F\u09BE\u0987\u09AE \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7\u09B0 (\u09AF\
  \u09C7\u09AE\u09A8 Node.js) \u0989\u09AA\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\
  \ \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.782057-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u098F\u0995\u099F\u09BF JavaScript-\u098F\u09B0 \u09B8\u09C1\
  \u09AA\u09BE\u09B0\u09B8\u09C7\u099F \u09B9\u0993\u09AF\u09BC\u09BE\u09AF\u09BC\
  , stderr \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AD\u09BF\u09A4\
  \u09CD\u09A4\u09BF JS \u09B0\u09BE\u09A8\u099F\u09BE\u0987\u09AE \u09AA\u09B0\u09BF\
  \u09AC\u09C7\u09B6\u09C7\u09B0 (\u09AF\u09C7\u09AE\u09A8 Node.js) \u0989\u09AA\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF \u098F\u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
TypeScript, একটি JavaScript-এর সুপারসেট হওয়ায়, stderr লেখার জন্য ভিত্তি JS রানটাইম পরিবেশের (যেমন Node.js) উপর নির্ভর করে। এখানে আপনি কিভাবে সরাসরি এটি করতে পারেন:

```typescript
console.error("এটি একটি এরর মেসেজ।");
```

stderr-এ নমুনা আউটপুট:
```
এটি একটি এরর মেসেজ।
```

Node.js পরিবেশে, আপনি আরও নিম্ন-স্তরের লেখার জন্য `process.stderr.write()` পদ্ধতি ব্যবহার করতে পারেন:

```typescript
process.stderr.write("নিম্ন স্তরের এরর মেসেজ।\n");
```

stderr-এ নমুনা আউটপুট:
```
নিম্ন স্তরের এরর মেসেজ।
```

আরও কাঠামোবদ্ধ এরর লগিংয়ের জন্য, আপনি `winston` অথবা `pino` এর মতো জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করতে পারেন। এখানে আপনি `winston` ব্যবহার করে কীভাবে এরর লগ করবেন:

প্রথমে, `winston` ইনস্টল করুন:

```bash
npm install winston
```

তারপর আপনার TypeScript ফাইলে এটি ব্যবহার করুন:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('winston ব্যবহার করে এরর লগ করা হয়েছে।');
```

এটি কনসোল এবং `error.log` নামের একটি ফাইলে এরর লিখবে। মনে রাখবেন, ফাইলে লেখার সময়, ডিস্কের স্পেস.ব্যবহার সম্পর্কিত সমস্যা এড়ানোর জন্য ফাইলের অনুমতিগুলি এবং রোলওভার পরিচালনা করা গুরুত্বপূর্ণ।
