---
title:                "ডিরেক্টরি আছে কিনা পরীক্ষা করা"
date:                  2024-03-17T17:45:24.066544-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
TypeScript এ ডিরেক্টরি অস্তিত্ব চেক করা গুরুত্বপূর্ণ ফাইল ম্যানেজমেন্ট কাজের জন্য, যেমন ফাইলগুলিতে ডাটা পড়া বা লেখা, সুনির্দিষ্ট ডিরেক্টরিতে কেবল অপারেশন সম্পাদনের নিশ্চিত করা। অস্তিত্ব নেই এমন ডিরেক্টরিতে অ্যাক্সেস বা ম্যানিপুলেট করার চেষ্টা থেকে উত্থাপিত ত্রুটি এড়াতে এই অপারেশন অত্যন্ত ক্রুশিয়াল।

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
