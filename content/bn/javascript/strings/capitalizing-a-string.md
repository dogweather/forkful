---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:33.346298-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7, \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\
  \u09A8\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\
  \u09A8\u09CB \u0986\u0997\u09C7 \u09A5\u09C7\u0995\u09C7 \u09A4\u09C8\u09B0\u09BF\
  \ \u09AE\u09C7\u09A5\u09A1 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u09AC\u09C7\u09B8\
  \u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8 \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\
  \u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\u09BC\u09A8\u2026"
lastmod: '2024-04-05T21:53:53.062573-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7, \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\u09A8\u09C0\u0995\u09B0\u09A3\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u0986\u0997\u09C7 \u09A5\u09C7\
  \u0995\u09C7 \u09A4\u09C8\u09B0\u09BF \u09AE\u09C7\u09A5\u09A1 \u09A8\u09C7\u0987\
  , \u09A4\u09AC\u09C7 \u09AC\u09C7\u09B8\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\
  \ \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\
  \u09AC\u09BE\u09AF\u09BC\u09A8 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
জাভাস্ক্রিপ্টে, সরাসরি স্ট্রিং মূলধনীকরণের জন্য কোনো আগে থেকে তৈরি মেথড নেই, তবে বেসিক স্ট্রিং ম্যানিপুলেশন মেথডগুলি ব্যবহার করে এটি বাস্তবায়ন করা সহজ।

### স্ট্যান্ডার্ড জাভাস্ক্রিপ্ট ব্যবহার করে
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // আউটপুট: "Hello world"
```

### ES6 সংস্করণ
ES6 টেমপ্লেট লিটারাল ব্যবহার করে, ফাংশনটি আরও সংক্ষিপ্তভাবে লিখা যেতে পারে:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // আউটপুট: "Hello ES6"
```

### Lodash ব্যবহার করে
Lodash একটি জনপ্রিয় তৃতীয়-পক্ষের ইউটিলিটি লাইব্রেরি, যা জাভাস্ক্রিপ্ট মানগুলি, স্ট্রিং সহ, ম্যানিপুলেট এবং কাজ করার জন্য বিস্তৃত পরিসরের ফাংশন প্রদান করে। Lodash ব্যবহার করে স্ট্রিং মূলধনীকরণ করতে:
```javascript
// প্রথমে, যদি আপনি ইনস্টল না করে থাকেন: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH উদাহরণ')); // আউটপুট: "Lodash উদাহরণ"
```
_লক্ষ্য করুন কিভাবে Lodash শুধুমাত্র প্রথম অক্ষরটিকে মূলধনীকৃত করে না বরং বাকি স্ট্রিংটিকে ছোট হাতের অক্ষরে রূপান্তরিত করে, যা সাধারণ জাভাস্ক্রিপ্ট বাস্তবায়ন থেকে সামান্য ভিন্ন।_

### CSS ব্যবহার করে (শুধুমাত্র ডিসপ্লে উদ্দেশ্যে)
যদি লক্ষ্য হয় UI তে টেক্সটকে মূলধনীকৃত করে প্রদর্শন করা, CSS ব্যবহার করা যেতে পারে:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- প্রদর্শন করা হবে "Hello css" -->
```
**নোট:** এই পদ্ধতিটি ওয়েবপেজে টেক্সটের দৃশ্যমানতা পরিবর্তন করে তবে জাভাস্ক্রিপ্টে স্ট্রিং নিজেই পরিবর্তন করে না।
