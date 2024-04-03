---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:47.188586-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript \u09A8\u09BF\u099C\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\
  \u09B0\u09C7\u09B6\u09A8 \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09C7\
  \ \u09A8\u09BE \u0995\u09BE\u09B0\u09A3 \u098F\u099F\u09BF JavaScript-\u098F \u0995\
  \u09AE\u09CD\u09AA\u09BE\u0987\u09B2 \u09B9\u09AF\u09BC, \u09AF\u09BE \u0990\u09A4\
  \u09BF\u09B9\u09CD\u09AF\u0997\u09A4 \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09B0\
  \u09BE\u0989\u099C\u09BE\u09B0\u09C7 \u099A\u09BE\u09B2\u09BE\u09A8\u09CB \u09B9\
  \u09AF\u09BC \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.784110-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u09A8\u09BF\u099C\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u09B8\u09AE\
  \u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A8\u09BE \u0995\u09BE\u09B0\u09A3\
  \ \u098F\u099F\u09BF JavaScript-\u098F \u0995\u09AE\u09CD\u09AA\u09BE\u0987\u09B2\
  \ \u09B9\u09AF\u09BC, \u09AF\u09BE \u0990\u09A4\u09BF\u09B9\u09CD\u09AF\u0997\u09A4\
  \ \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09B0\u09BE\u0989\u099C\u09BE\u09B0\u09C7\
  \ \u099A\u09BE\u09B2\u09BE\u09A8\u09CB \u09B9\u09AF\u09BC \u09AF\u09C7\u0996\u09BE\
  \u09A8\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7 \u09B8\u09C0\u09AE\u09BF\u09A4 \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6\u09BE\
  \u09A7\u09BF\u0995\u09BE\u09B0 \u09A5\u09BE\u0995\u09C7\u0964 \u09A4\u09AC\u09C7\
  , Node.js \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09C3\u09A4 \u09B9\u09B2\u09C7, `fs` \u09AE\u09A1\u09BF\u0989\u09B2 (File System)\
  \ \u09AB\u09BE\u0987\u09B2 \u09B2\u09BF\u0996\u09A8\u09C7\u09B0 \u09B8\u09C1\u09AC\
  \u09BF\u09A7\u09BE \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  \n\n#."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
TypeScript নিজে সরাসরি ফাইল অপারেশন সমাধান করে না কারণ এটি JavaScript-এ কম্পাইল হয়, যা ঐতিহ্যগত ভাবে ব্রাউজারে চালানো হয় যেখানে ফাইল সিস্টেমে সীমিত প্রবেশাধিকার থাকে। তবে, Node.js পরিবেশে ব্যবহৃত হলে, `fs` মডিউল (File System) ফাইল লিখনের সুবিধা প্রদান করে।

### Node.js fs মডিউল ব্যবহার করে
প্রথমত, আপনি যে Node.js পরিবেশে কাজ করছেন তা নিশ্চিত করুন। এরপর, টেক্সট ফাইল লিখতে `fs` মডিউল ব্যবহার করুন। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

```typescript
import * as fs from 'fs';

const data = 'হ্যালো, বিশ্ব!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('ফাইল সংরক্ষণ করা হয়েছে!');
});
```

এটি অসিঙ্ক্রোনাসভাবে "হ্যালো, বিশ্ব!" কে `message.txt` তে লিখবে। যদি ফাইলটি অস্তিত্বে না থাকে, Node.js এটি তৈরি করে; যদি থাকে, Node.js এটি ওভাররাইট করে।

সিঙ্ক্রোনাস ফাইল লেখার জন্য, `writeFileSync` ব্যবহার করুন:

```typescript
import * as fs from 'fs';

const data = 'আবার হ্যালো, বিশ্ব!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('ফাইল সংরক্ষণ করা হয়েছে!');
} catch (err) {
    console.error(err);
}
```

### জনপ্রিয় থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করা
নেটিভ `fs` মডিউল শক্তিশালী হওয়া সত্ত্বেও, কিছু ডেভেলপার অতিরিক্ত সুবিধা ও ফাংশনালিটির জন্য থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করতে পছন্দ করে। `fs-extra` একটি জনপ্রিয় পছন্দ যা `fs` কে প্রসারিত করে এবং ফাইল অপারেশনগুলি আরও সহজ করে দেয়।

প্রথমে, আপনাকে `fs-extra` ইনস্টল করতে হবে:

```
npm install fs-extra
```

এরপর, আপনি টেক্সট কন্টেন্ট লিখতে আপনার TypeScript ফাইলে এটি ব্যবহার করতে পারেন:

```typescript
import * as fs from 'fs-extra';

const data = 'এটি fs-extra!';
const filePath = './extraMessage.txt';

// অ্যাসিংক/await ব্যবহার করে
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('fs-extra দিয়ে ফাইল সংরক্ষণ করা হয়েছে!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

এই কোড স্নিপেটটি পূর্বের `fs` উদাহরণগুলির মতোই কাজ করে কিন্তু `fs-extra` লাইব্রেরি ব্যবহার করে, যা প্রতিশ্রুতিগুলি সামলানোর জন্য একটি পরিষ্কার সিনট্যাক্স অফার করে।
