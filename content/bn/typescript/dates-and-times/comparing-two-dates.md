---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.411344-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09B8\u09AE\u09AF\u09BC\u0995\u09CD\u09B0\u09AE\u09BF\u0995 \u09B8\u09AE\
  \u09CD\u09AA\u09B0\u09CD\u0995 \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\
  \u09B0\u09BE\u2014\u09A4\u09BE\u09B0\u09BE \u0995\u09BF \u098F\u0995\u0987, \u098F\
  \u0995\u099F\u09BF \u0995\u09BF \u0985\u09A8\u09CD\u09AF\u09C7\u09B0 \u099A\u09C7\
  \u09AF\u09BC\u09C7 \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7, \u09A8\u09BE\u0995\u09BF\
  \ \u09AA\u09B0\u09C7? \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BE \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0\
  \ \u09B8\u09C2\u099A\u09BF\u09A4 \u0995\u09B0\u09BE,\u2026"
lastmod: '2024-03-17T18:47:43.778026-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09B8\u09AE\u09AF\u09BC\u0995\u09CD\u09B0\u09AE\u09BF\u0995 \u09B8\u09AE\u09CD\
  \u09AA\u09B0\u09CD\u0995 \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\u09B0\
  \u09BE\u2014\u09A4\u09BE\u09B0\u09BE \u0995\u09BF \u098F\u0995\u0987, \u098F\u0995\
  \u099F\u09BF \u0995\u09BF \u0985\u09A8\u09CD\u09AF\u09C7\u09B0 \u099A\u09C7\u09AF\
  \u09BC\u09C7 \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7, \u09A8\u09BE\u0995\u09BF \u09AA\
  \u09B0\u09C7? \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BE \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0 \u09B8\
  \u09C2\u099A\u09BF\u09A4 \u0995\u09B0\u09BE,\u2026"
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

দুটি তারিখের তুলনা মানে তাদের সময়ক্রমিক সম্পর্ক নির্ণয় করা—তারা কি একই, একটি কি অন্যের চেয়ে পূর্বে, নাকি পরে? প্রোগ্রামাররা এটা ঘটনাবলী সূচিত করা, সময়রেখা সাজানো, এবং সময়কাল যাচাই করার জন্য করে থাকেন।

## কিভাবে করবেন:

চলুন কিছু তারিখ তুলনা করি:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// তারিখ ১ কি তারিখ ২-এর আগে?
console.log(date1 < date2); // সত্য

// তারিখ ১ কি তারিখ ২-এর সমান?
console.log(date1.getTime() === date2.getTime()); // মিথ্যা

// কত দিনের ব্যবধান?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // ১
```

নমুনা আউটপুট:

```
সত্য
মিথ্যা
১
```

## গভীরে যাওয়া

পূর্বের দিনগুলিতে, তারিখগুলো বিভিন্ন ফর্ম্যাটের এবং জটিল গণনার একটা গোলমেলে ছিল। জাভাস্ক্রিপ্ট (এবং টাইপস্ক্রিপ্ট দ্বারা প্রত্যাহারে) `Date` অবজেক্টের মাধ্যমে জিনিসগুলি সরলীকৃত হয়েছে, আমাদের সময় নিয়ে কাজ করার মানদণ্ড নির্ধারণ করে।

বিকল্প? নিশ্চয়ই। `moment.js` বা `date-fns` এর মতো লাইব্রেরিগুলি অতিরিক্ত কার্যকারিতার সাথে তারিখ পরিচালনা বাড়ায়। কিন্তু মৌলিক তুলনার জন্য? নেটিভ Date-এর সাধারণতা প্রায়শই কাজ করে।

অন্তরালে, `Date.getTime()` মিলিসেকেন্ডকে প্রাপ্ত হয়, এপোক থেকে (জানুয়ারি ১, ১৯৭০)। এই মূল্যগুলির সাথে তুলনা করে সময়ক্ষেত্রের প্রতিকূলতা এবং লিপ সেকেন্ডের গুল্মগুলি পরিষ্কার করে দেয়, এটি সংখ্যার দিকে নামিয়ে আনে।

## দেখুন এছাড়াও

- [মোজিলা ডেভেলপার নেটওয়ার্ক তারিখ রেফারেন্স](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) তারিখ অবজেক্টগুলির বিস্তারিত জানার জন্য।
- [তুমি মোমেন্ট.জেএস প্রয়োজন নেই](https://github.com/you-dont-need/You-Dont-Need-Momentjs) লাইব্রেরি চাই কি চাই না তা নির্ণয়ের জন্য।
- [টাইপস্ক্রিপ্ট অফিশিয়াল ডকুমেন্টেশন](https://www.typescriptlang.org/docs/) টাইপস্ক্রিপ্টের শক্তি এবং দুর্বলতা সম্পর্কে আরও জানার জন্য।
