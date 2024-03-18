---
title:                "টেক্সট ফাইল পড়া"
date:                  2024-03-17T18:09:38.927142-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি টেক্সট ফাইল পড়া মানে হল এমন একটি ফাইলের বিষয়বস্তু অর্জন করা যা মানুষের পড়ার উপযোগী হিসেবে গঠিত। প্রোগ্রামাররা এটি ডেটা প্রক্রিয়াজাত বা বিশ্লেষণ করতে, যেমন কনফিগারেশন পড়া, ডেটা আমদানি করা, অথবা কেবল উপাদানগুলো একটি অ্যাপ্লিকেশন দ্বারা প্রক্রিয়াজাত করার জন্য গ্রহণ করে থাকেন।

## কিভাবে:

চলুন, Node.js's `fs/promises` মডিউল ব্যবহার করে TypeScript এ একটি টেক্সট ফাইল পড়ি। আমরা এই উদাহরণটি সাধারণ রাখব: `example.txt` নামে একটি ফাইল পড়ে এর বিষয়বস্তু লগ করুন।

```typescript
import { readFile } from 'fs/promises';

async function readTextFile(filePath: string) {
  try {
    const data = await readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error(`Error reading file from disk: ${error}`);
  }
}

readTextFile('./example.txt');
```

নমুনা আউটপুট:
```
হ্যালো, ফাইল থেকে আসা বিষয়বস্তু!
```

## গভীর ডুব

ঐতিহাসিকভাবে, Node.js এ ফাইল পড়া অনেকটা কলব্যাক ভিত্তিক ছিল, যা "কলব্যাক নরক" নামে পরিচিত একটি ঘটনাকে ডাকা হতে পারে। প্রমিস এবং `async/await` এর আবির্ভাবের সাথে সাথে এই প্রক্রিয়াটি অনেক বেশি সহজ হয়ে গেছে।

`fs/promises` ছাড়াও, পুরানো `fs` মডিউল রয়েছে যা এখনও কলব্যাক প্যাটার্ন সাথে কাজ করে। `fs.createReadStream()` এর সাথে স্ট্রিম প্রসেসিং ব্যবহারের অপশনও আছে, যা বড় ফাইলের জন্য কম মেমোরি খরচের কারণে উপকারী।

প্রয়োগের দিক থেকে, ফাইল সিস্টেমে অ্যাক্সেস একটি ইনপুট/আউটপুট অপারেশন এবং স্বাভাবিকভাবে মেমোরির মধ্যে অপারেশনের তুলনায় ধীর। এটি কেন অ্যাসিনক্রোনাস কোডিং প্যাটার্নগুলো গুরুত্বপূর্ণ — তারা মূল থ্রেডকে ব্লক করা থেকে বিরত রাখে এবং Node.js কে অন্যান্য কাজ সামলাতে সহায়তা করে।

## আরও দেখুন

Node.js ফাইল সিস্টেমে গভীর ডুবের জন্য:
- Node.js fs ডকুমেন্টেশন: https://nodejs.org/api/fs.html
- `fs/promises` বুঝতে: https://nodejs.org/dist/latest/docs/api/fs.html#filehandlepromises
- স্ট্রিম-ভিত্তিক ফাইল পড়া: https://nodejs.org/api/stream.html#stream
TypeScript সম্পর্কিত রিসোর্স সামগ্রী:
- TypeScript গভীর ডুব: https://basarat.gitbook.io/typescript/
- TypeScript হ্যান্ডবুক: https://www.typescriptlang.org/docs/handbook/intro.html
