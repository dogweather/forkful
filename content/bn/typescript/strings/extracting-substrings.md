---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:17.933071-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F, \u0986\u09AA\
  \u09A8\u09BF `substring()`, `slice()`, \u098F\u09AC\u0982 ES6 '\u098F\u09B0 `includes()`\
  \ \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09BE\u099F\u09BE\u0995\u09BE\
  \u099F\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09BE\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09B8\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:43.753289-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F, \u0986\u09AA\u09A8\u09BF `substring()`, `slice()`, \u098F\
  \u09AC\u0982 ES6 '\u098F\u09B0 `includes()` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0995\u09BE\u099F\u09BE\u0995\u09BE\u099F\u09BF \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u09B8\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE\u09B0 \u0995\u09BE\u099C\u09C7 \u0986\
  \u09B8\u09C7\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
TypeScript-এ, আপনি `substring()`, `slice()`, এবং ES6 'এর `includes()` মেথডের মাধ্যমে স্ট্রিং কাটাকাটি করতে পারেন যা স্ট্রিংসের মধ্যে টেক্সট খোঁজার কাজে আসে।

```TypeScript
let fullString: string = "Hello, TypeScript enthusiasts!";

// অক্ষর 7 থেকে 18 পর্যন্ত নিন
let substr: string = fullString.substring(7, 18);
console.log(substr); // আউটপুট দেয়: TypeScript

// slice() দিয়ে একই কাজ
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // আউটপুট দেয়: TypeScript

// একটি সাবস্ট্রিং এর অস্তিত্ব পরীক্ষা করা
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // আউটপুট দেয়: true
```

## গভীরে যাওয়া
একসময়, স্ট্রিং ম্যানিপুলেশন আরও জটিল ছিল—C এর স্ট্রিং ফাংশনগুলির কথা ভাবুন। এখন, JavaScript এবং TypeScript ইউনিকোড পরিচালনা, অক্ষর এনকোডিং সম্মান করা এবং সরাসরি স্ট্রিং অবজেক্টের সাথে কাজ করা সম্পর্কিত পদ্ধতি অফার করে। `substring()` এবং `slice()` এর মধ্যে সামান্য পার্থক্য আছে: `slice()` ঋণাত্মক সূচী নিতে পারে, শেষ থেকে প্রত্যাহারে গণনা করে। `substring()` তাদের শূন্য হিসেবে ব্যবহার করে। পারফরম্যান্স-স্পর্শকাতর পরিস্থিতিতে, একটিকে অন্যের উপর প্রাধান্য দেওয়া গুরুত্ব পেতে পারে, তবে দৈনন্দিন ব্যবহারের জন্য, এটি মোটামুটি একইরকম।

```TypeScript
// slice দিয়ে ঋণাত্মক সূচী ব্যবহার
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // আউটপুট দেয়: Hello, Type
```

`includes()` এর ক্ষেত্রে, এটি ক্লাসিক `indexOf()` এর উপরে পাঠযোগ্যতা উন্নতি সাধন করে, আপনার উদ্দেশ্য একনজরে পরিষ্কার করে। আর নেই `if (string.indexOf('some text') !== -1)`; কেবল সরাসরি একটি `if (string.includes('some text'))` ব্যবহার।

## আরও দেখুন
- স্ট্রিং টাইপ্স কিভাবে ব্যবহার করা যায় তার উপরে TypeScript হ্যান্ডবুক: [TypeScript String](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- JavaScript-এ স্ট্রিং পদ্ধতি সম্পর্কে MDN Web Docs, TypeScript এর জন্য প্রযোজ্য: [MDN String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Unicode ও JavaScript (অতএব TypeScript) সম্পর্কে আরও জানার জন্য, দেখুন [Understanding JavaScript's internal character encoding: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)
