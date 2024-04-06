---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:24.066544-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Node.js \u098F\u09A8\u09AD\u09BE\
  \u09AF\u09BC\u09B0\u09A8\u09AE\u09C7\u09A8\u09CD\u099F\u09C7 \u099A\u09BE\u09B2\u09BF\
  \u09A4 TypeScript `fs` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\
  \u09CD\u09AC \u099A\u09C7\u0995 \u0995\u09B0\u09A4\u09C7 \u09B8\u0995\u09CD\u09B7\
  \u09AE, \u09AF\u09BE `existsSync()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09BE\u2026"
lastmod: '2024-04-05T21:53:51.933886-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u098F\u09A8\u09AD\u09BE\u09AF\u09BC\u09B0\u09A8\u09AE\u09C7\u09A8\
  \u09CD\u099F\u09C7 \u099A\u09BE\u09B2\u09BF\u09A4 TypeScript `fs` \u09AE\u09A1\u09BF\
  \u0989\u09B2\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\
  \u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u099A\u09C7\u0995\
  \ \u0995\u09B0\u09A4\u09C7 \u09B8\u0995\u09CD\u09B7\u09AE, \u09AF\u09BE `existsSync()`\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09BE \u0985\u09CD\u09AF\u09BE\u09B8\u09BF\
  \u0999\u09CD\u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8 `access()` \u09AB\u09BE\u0982\
  \u09B6\u09A8\u0995\u09C7 `constants.F_OK` \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u099C\u09C1\u09A1\u09BC\u09C7 \u09A6\u09BF\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u0964."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
Node.js এনভায়রনমেন্টে চালিত TypeScript `fs` মডিউলের ব্যবহার করে একটি ডিরেক্টরির অস্তিত্ব চেক করতে সক্ষম, যা `existsSync()` ফাংশন বা অ্যাসিঙ্ক্রোনাস `access()` ফাংশনকে `constants.F_OK` এর সাথে জুড়ে দিতে পারে।

### `fs.existsSync()` ব্যবহার করে:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('ডিরেক্টরি অস্তিত্ব আছে।');
} else {
  console.log('ডিরেক্টরি অস্তিত্ব নেই।');
}
```

### `fs.access()` `fs.constants.F_OK` এর সাথে ব্যবহার করে:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('ডিরেক্টরি অস্তিত্ব নেই।');
    return;
  }
  console.log('ডিরেক্টরি অস্তিত্ব আছে।');
});
```

**ধরে নেওয়া হলে** উভয় পদ্ধতির জন্য নমুনা আউটপুট, যে ডিরেক্টরি অস্তিত্ব আছে:
```
ডিরেক্টরি অস্তিত্ব আছে।
```

এবং যদি না থাকে:
```
ডিরেক্টরি অস্তিত্ব নেই।
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার - `fs-extra`:
`fs-extra` হল একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি যা বিল্ট-ইন `fs` মডিউলকে উন্নত করে এবং আরো সুবিধাজনক ফাংশন প্রদান করে।

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`ডিরেক্টরি অস্তিত্ব আছে: ${exists}`);
});
```

**ধরে নেওয়া হলে** নমুনা আউটপুট, যে ডিরেক্টরি অস্তিত্ব আছে:
```
ডিরেক্টরি অস্তিত্ব আছে: সত্যি
```

এবং যদি না থাকে:
```
ডিরেক্টরি অস্তিত্ব নেই: মিথ্যা
```
