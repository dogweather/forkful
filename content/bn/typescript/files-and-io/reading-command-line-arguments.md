---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:41.935661-06:00
description: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\
  \u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8\u0997\u09C1\u09B2\u09CB\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\u09C7\
  \u09B0 \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B8 \u0995\u09B0\u09A4\u09C7\
  \ \u09A6\u09C7\u09AF\u09BC \u09AF\u0996\u09A8 \u09A4\u09BE\u09B0\u09BE \u098F\u099F\
  \u09BF \u099A\u09BE\u09B2\u09BE\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.781054-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8\u0997\u09C1\u09B2\u09CB \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\u09C7\u09B0 \u098F\
  \u0995\u099F\u09BF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B8 \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\
  \u09AF\u09BC \u09AF\u0996\u09A8 \u09A4\u09BE\u09B0\u09BE \u098F\u099F\u09BF \u099A\
  \u09BE\u09B2\u09BE\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\
  \u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0\u2026"
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কি এবং কেন?
কমান্ড লাইন আর্গুমেন্টসগুলো ব্যবহারকারীদের একটি প্রোগ্রামে ডেটা পাস করতে দেয় যখন তারা এটি চালান। প্রোগ্রামাররা কোড পরিবর্তন না করে একটি প্রোগ্রামের আচরণকে কাস্টমাইজ করতে তাদের ব্যবহার করে।

## কিভাবে:

TypeScript-এ, আপনি Node.js ব্যবহার করে কমান্ড লাইন আর্গুমেন্টস পড়তে পারেন। এর পদ্ধতি এরকম:

```typescript
// Node.js থেকে process ইমপোর্ট করার জন্য প্রয়োজন
import process from 'process';

// তৃতীয় অবস্থান থেকে কমান্ড লাইন আর্গুমেন্টস গ্রহণ
const args = process.argv.slice(2);

console.log('কমান্ড লাইন আর্গুমেন্টস:', args);
```

এই স্ক্রিপ্টটি `ts-node yourscript.ts arg1 arg2` হিসেবে রান করুন এবং দেখুন:

```
কমান্ড লাইন আর্গুমেন্টস: ['arg1', 'arg2']
```

## গভীরে যাচাই

প্রারম্ভিক কমান্ড লাইন দিনগুলিতে ফিরে, ব্যবহারকারী ইন্টারেকশন পুরোপুরি টেক্সট সম্পর্কিত ছিল। লিনাক্স, UNIX, এবং Windows প্রোগ্রামগুলিকে কি করতে হবে তা বলার জন্য কমান্ড লাইন আর্গুমেন্টস ব্যবহার করত।

এখন বিকল্পগুলোর জন্য: `process.argv` ছাড়াও, Node.js-এ, আপনি `yargs` অথবা `commander` এর মত লাইব্রেরিগুলো ব্যবহার করতে পারেন পার্সিং এবং ভ্যালিডেশনের মত আরও অনেক বৈশিষ্ট্যের জন্য।

TypeScript-এ এর মূলভাব সাধারণ: `process.argv` একটি অ্যারে সব আর্গুমেন্টস ধারণ করে। ইনডেক্স 0 নোডের পথ, ইনডেক্স 1 স্ক্রিপ্টের পথ, তাই আসল আর্গুমেন্টস ইনডেক্স 2 থেকে শুরু হয়।

## আরও দেখুন

আরও গভীরে যাওয়ার জন্য, এগুলো দিয়ে শুরু করুন:

- [Node.js process.argv ডকুমেন্টেশন](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub রিপোজিটরি](https://github.com/yargs/yargs)
- [Commander.js GitHub রিপোজিটরি](https://github.com/tj/commander.js)
