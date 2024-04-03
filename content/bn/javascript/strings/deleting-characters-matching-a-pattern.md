---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:24.169668-06:00
description: "\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u0985\u09A8\u09C1\
  \u09AF\u09BE\u09DF\u09C0 \u0985\u0995\u09CD\u09B7\u09B0 \u09AE\u09C1\u099B\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7\
  \ \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09A4\u09B2 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\
  \u0982 \u099C\u09A8\u09CD\u09AF, \u0985\u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8\u09C0\u09DF \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u0995\u09B0\u09A3\u09C7\u09B0 \u0986\u0997\
  \u09C7\u2026"
lastmod: '2024-03-17T18:47:44.436360-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u0985\u09A8\u09C1\
  \u09AF\u09BE\u09DF\u09C0 \u0985\u0995\u09CD\u09B7\u09B0 \u09AE\u09C1\u099B\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7\
  \ \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09A4\u09B2 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\
  \u0982 \u099C\u09A8\u09CD\u09AF, \u0985\u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8\u09C0\u09DF \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u0995\u09B0\u09A3\u09C7\u09B0 \u0986\u0997\
  \u09C7 \u09B8\u09B0\u09B2\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
নিয়মিত এক্সপ্রেশন সহ `replace()` ব্যবহার করুন। `g` ফ্ল্যাগটি প্রথম মিল ছাড়া সব মিল প্রতিস্থাপন করে।

```javascript
let message = "S0m3 messy-string_with! unwanted characters.";
let cleanMessage = message.replace(/[0-9_!-]/g, '');
console.log(cleanMessage); // আউটপুট: "Sm messystringwith unwanted characters."
```

## গভীরে যাওয়া
জাভাস্ক্রিপ্ট দীর্ঘদিন ধরে প্যাটার্ন ম্যাচিংয়ের জন্য নিয়মিত এক্সপ্রেশন (`RegExp`) ব্যবহার করে চলেছে। `replace()` ফাংশনটি স্ট্রিং পরিবর্তনের জন্য আপনার যেতে হবে স্থান যেহেতু এটি ভাষার প্রারম্ভিক দিনগুলিতেই প্রবর্তিত হয়েছে। `split()` এবং `join()` বা স্ট্রিংগুলি পুনর্নির্মাণের জন্য লুপ ব্যবহার করা একটি সম্ভাবনা আছে কিন্তু এগুলি ততোটা সংক্ষিপ্ত নয়।

এখানে একটি ব্রেকডাউন:
- সোজা, এক-লাইনের সমাধানের জন্য `replace()` ব্যবহার করুন।
- নিয়মিত এক্সপ্রেশনে শক্তিশালী প্যাটার্ন-ম্যাচিং সুবিধা রয়েছে।
- বৃহৎ স্ট্রিং অথবা ঘন ঘন লুপে `RegExp`-এর পারফরম্যান্সের উপর সচেতন থাকুন।

আধুনিক অনুশীলনের উপর একটি কথা: প্যাটার্নগুলি যেমন `/[^a-z]/gi` অক্ষর ছাড়া সবকিছু মুছে ফেলে, `i` ফ্ল্যাগের সাথে কেস-সংবেদনশীলতা সম্মান করে। ECMAScript 2015-এ টেমপ্লেট লিটেরালের প্রবর্তনে জটিল প্রতিস্থাপনগুলি সহজ করেছে, পাঠযোগ্যতা বাড়িয়েছে।

নিয়মিত এক্সপ্রেশন এখনও কিছু প্রোগ্রামারের তাদের সংকেত জটিলতার কারণে ভীতি তৈরি করে। তবে, আধুনিক জাভাস্ক্রিপ্টের বিকাশে, স্ট্রিং ম্যানিপুলেশন ফাংশনগুলি (`trim()`, `padStart()`, `padEnd()` ইত্যাদির মতো) যেমন টুলস ও পদ্ধতিগুলি রেগেক্স ছাড়াই সাধারণ কাজের সরলীকরণের জন্য উপলব্ধ।

## আরও দেখুন
- [MDN Web Docs on replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExr: Learn, build, & test RegEx](https://regexr.com/)
- [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
