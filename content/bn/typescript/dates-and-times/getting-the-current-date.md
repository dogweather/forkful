---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:51.807963-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F, \u0986\u09AA\
  \u09A8\u09BF `Date` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\
  \u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\
  \ \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
lastmod: '2024-03-17T18:47:43.775789-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F, \u0986\u09AA\u09A8\u09BF `Date` \u0985\u09AC\u099C\u09C7\
  \u0995\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\
  \u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  ."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
TypeScript-এ, আপনি `Date` অবজেক্ট ব্যবহার করে বর্তমান তারিখ এবং সময় পেতে পারেন। এখানে কিভাবে এটি করতে পারেন:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

নমুনা আউটপুট:
```
2023-04-12T07:20:50.52Z
```

এই কোড স্নিপেটটি একটি নতুন `Date` অবজেক্ট তৈরি করে, যা বর্তমান তারিখ এবং সময় ধারণ করে, যা তারপর কনসোলে প্রিন্ট করা হয়। আপনি toLocaleDateString() ব্যবহার করে তারিখটি আরও পড়ার উপযোগী ফরমেটে ফরম্যাট করতে পারেন:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

নমুনা আউটপুট:
```
4/12/2023
```

### date-fns ব্যবহার করে
বিস্তারিত তারিখ পরিবর্তন এবং ফরম্যাটিং এর জন্য, `date-fns` লাইব্রেরিটি একটি জনপ্রিয় পছন্দ। প্রথমে, এটি npm এর মাধ্যমে ইনস্টল করুন:

```bash
npm install date-fns
```

তারপর, আপনি এটি ব্যবহার করে বর্তমান তারিখটি ফরম্যাট করতে পারেন:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

নমুনা আউটপুট:
```
2023-04-12
```

এই `date-fns` উদাহরণটি বর্তমান তারিখটিকে "YYYY-MM-DD" ফরম্যাটে একটি স্ট্রিং হিসেবে ফরম্যাট করে। লাইব্রেরিটি তারিখ পরিবর্তনের জন্য বিপুল সংখ্যক ফাংশন অফার করে, যা তারিখের সাথে কাজ করা যেকোনো TypeScript প্রোগ্রামারের জন্য একটি বহুমুখী টুল করে তোলে।
