---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:00.621351-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script, \u099C\u09BE\
  \u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u0989\
  \u09AA\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09BE\u09AF\u09BC, \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\u09A8\u09C0\u0995\u09B0\u09A3\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\
  , \u09AF\u09A6\u09BF\u0993 \u098F\u099F\u09BF\u09A4\u09C7 \u0995\u09CB\u09A8\u09CB\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.504305-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u0989\u09AA\u09B0 \u09AD\u09BF\u09A4\u09CD\
  \u09A4\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09BE\u09AF\u09BC, \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\
  \u09A8\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\
  \u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B8\u09B0\
  \u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7, \u09AF\u09A6\u09BF\u0993 \u098F\u099F\
  \u09BF\u09A4\u09C7 \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\
  \u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09C7\u0987\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0995\u09AF\u09BC\u09C7\u0995\u099F\u09BF \u09B8\u0982\u0995\u09CD\
  \u09B7\u09BF\u09AA\u09CD\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
Google Apps Script, জাভাস্ক্রিপ্টের উপর ভিত্তি করে থাকায়, একটি স্ট্রিং মূলধনীকরণের জন্য বেশ কিছু পদ্ধতি সরবরাহ করে, যদিও এটিতে কোনো বিল্ট-ইন ফাংশন নেই। এখানে কয়েকটি সংক্ষিপ্ত উদাহরণ দেওয়া হল:

**পদ্ধতি ১: charAt() এবং slice() ব্যবহার করে**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// নমুনা ব্যবহার
let result = capitalizeString('hello, world');
console.log(result);  // আউটপুট: Hello, world
```

**পদ্ধতি ২: একটি Regex ব্যবহার করে**

যারা এজ কেসগুলি আরও মার্জিতভাবে হ্যান্ডেল করার জন্য রেজেক্স-ভিত্তিক সমাধান পছন্দ করেন:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// নমুনা ব্যবহার
let result = capitalizeStringRegex('hello, world');
console.log(result);  // আউটপুট: Hello, world
```

উভয় পদ্ধতি নিশ্চিত করে যে স্ট্রিংয়ের প্রথম অক্ষর মূলধনীবদ্ধ এবং বাকি অক্ষরগুলি ছোট হাতের অক্ষরে থাকে, যা Google Sheets ম্যানিপুলেশন বা Apps Script এর মাধ্যমে ডকুমেন্ট এডিটিং সহ বিভিন্ন প্রয়োগের জন্য উপযুক্ত।

## গভীরে ডুব দিয়ে
Google Apps Script এ স্ট্রিংগুলিকে মূলধনীকরণ খুবই সরল, জাভাস্ক্রিপ্টের শক্তিশালী স্ট্রিং ম্যানিপুলেশন ক্ষমতাগুলির সুবিধা গ্রহণ করে। ঐতিহাসিকভাবে, পাইথনের মতো ভাষাগুলিতে `.capitalize()` এর মতো বিল্ট-ইন পদ্ধতিগুলি এটি অর্জনের জন্য রয়েছে, যা জাভাস্ক্রিপ্ট এবং Apps Script প্রোগ্রামারদের জন্য সামান্য অতিরিক্ত পদক্ষেপ স্থাপন করে। তবে, জাভাস্ক্রিপ্ট/Google Apps Script এ বিল্ট-ইন ফাংশন না থাকাটা ফ্লেক্সিবিলিটি এবং স্ট্রিং ম্যানিপুলেশন কৌশলগুলিতে গভীর উপলব্ধি প্রকাশ করার দিকে উৎসাহিত করে।

জটিল সিনারিওগুলিতে, যেমন একটি স্ট্রিংয়ের প্রতিটি শব্দকে মূলধনীকরণ করা (`টাইটেল কেস`), প্রোগ্রামাররা প্রতিটি শব্দকে পৃথকভাবে প্রক্রিয়া করার জন্য `split()` এবং `map()` ফাংশনগুলির সাথে রেজেক্স পদ্ধতিগুলি সংযুক্ত করতে পারে। যদিও Google Apps Script সরাসরি স্ট্রিং মূলধনীকরণের জন্য কোনো পদ্ধতি সরবরাহ করে না, বিদ্যমান জাভাস্ক্রিপ্ট স্ট্রিং ম্যানিপুলেশন পদ্ধতিগুলির ব্যবহার প্রচুর ফ্লেক্সিবিলিটি অফার করে, যা ডেভেলপারদের তাদের নির্দিষ্ট প্রয়োজন অনুসারে স্ট্রিংগুলিকে দক্ষতার সাথে হ্যান্ডেল করতে সাহায্য করে।

কর্মক্ষমতা এবং দক্ষতার ক্ষেত্রে যখন পরম গুরুত্ব পায়, তখন এটি উল্লেখ্য যে সরাসরি স্ট্রিং ম্যানিপুলেশন বড় স্ট্রিংগুলির জন্য বা বড় লুপের মধ্যে অপারেশনের জন্য রেজেক্সের তুলনায় অধিক কর্মক্ষম হতে পারে। তবে, Google Apps Script এর মধ্যে বেশিরভাগ বাস্তবিক প্রয়োগের জন্য, উভয় পদ্ধতিই নির্ভরযোগ্য সমাধান প্রদান করে।
