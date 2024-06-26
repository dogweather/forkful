---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:03.416362-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7, `String.prototype.replace()`\
  \ \u09B9\u099A\u09CD\u099B\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\
  \u09A5\u0964 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0985\u09A5\u09AC\u09BE \u09B0\u09C7\u0997\u09C7\u0995\u09CD\u09B8 \u098F\u09AC\
  \u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u099F\u09BF\
  \ \u09AA\u09BE\u09B8 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09A6\u09CD\u09B0\u09C1\u09A4 \u098F\u09AC\u0982 \u09A8\u09CB\u0982\u09B0\u09BE\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:44.437309-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7, `String.prototype.replace()` \u09B9\u099A\u09CD\u099B\u09C7 \u09AF\
  \u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\u09A5\u0964 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u09B0\u09C7\
  \u0997\u09C7\u0995\u09CD\u09B8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u099F\u09BF \u09AA\u09BE\u09B8 \u0995\u09B0\
  \u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09CD\u09B0\u09C1\u09A4\
  \ \u098F\u09AC\u0982 \u09A8\u09CB\u0982\u09B0\u09BE \u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
জাভাস্ক্রিপ্টে, `String.prototype.replace()` হচ্ছে যাওয়ার পথ। একটি স্ট্রিং অথবা রেগেক্স এবং প্রতিস্থাপনটি পাস করুন। এখানে দ্রুত এবং নোংরা উদাহরণ দেওয়া হল:

```javascript
let str = "I love to code in JavaScript!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // আউটপুট: I love to code in TypeScript!
```

এখন, গ্লোবাল প্রতিস্থাপনের জন্য রেগেক্স সহ:

```javascript
let story = "The quick brown fox jumps over the lazy dog. The fox is clever.";
let newStory = story.replace(/fox/g, "cat");
console.log(newStory); // আউটপুট: The quick brown cat jumps over the lazy dog. The cat is clever.
```

## গভীর ডুব
ফিরে দেখা, `String.prototype.replace()` জেএস-এর প্রাথমিক দিনগুলিতে ছিল—নেটস্কেপ ২ এর শুরু থেকে। এখন, ES6 আমাদের টেমপ্লেট লিটেরাল এবং এরো ফাংশন নিয়ে এসেছে, যা রেগেক্স ব্যবহারের মাধ্যমে আরও সংক্ষিপ্ত এবং পড়াযোগ্য কোডের অভিজ্ঞতা যোগ করে।

বিকল্প? নিশ্চিতভাবে। যদি আপনি বৃহৎ স্কেলের টেক্সট প্রসেসিং এর সাথে কাজ করতে যাচ্ছেন, তাহলে আপনি Node.js স্ট্রিমস-এর দিকে মনোনিবেশ করতে পারেন অথবা জটিল প্যাটার্ন, দক্ষতা, এবং কর্মক্ষমতা হ্যান্ডেল করার জন্য বাহ্যিক লাইব্রেরিগুলি ব্যবহার করতে পারেন।

বাস্তবায়নের ক্ষেত্রে, `replace()` এককভাবে সাধারণ। কিন্তু রেগেক্স প্যাটার্নগুলি জটিল হতে পারে। সহজ থেকে শুরু করুন, বিশেষ অক্ষরগুলি শিখুন (`.` যেকোন অক্ষরের সাথে ম্যাচ করে, `*` পুনরাবৃত্তি প্যাটার্নের জন্য), এবং regex101 এর মতো টুলের সাথে পরীক্ষা করুন।

## আরো দেখুন
- MDN replace নথিপত্র: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- এক্সপ্রেশন পরীক্ষা করার জন্য Regex101: https://regex101.com/
- রেগেক্স সম্পর্কে JavaScript তথ্য: https://javascript.info/regexp-introduction
