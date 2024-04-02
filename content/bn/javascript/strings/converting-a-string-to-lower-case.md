---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:43.532957-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2 \u098F\u09B0 \u09AD\u09C7\u09A4\u09B0\u09C7\u09B0 \u09B8\u09AE\u09B8\
  \u09CD\u09A4 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\
  \ \u09AA\u09CD\u09B0\u09A4\u09BF\u09B6\u09AC\u09CD\u09A6\u09C7 \u09AA\u09B0\u09BF\
  \u09A3\u09A4 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AE\u09C2\u09B2\u09A4\
  \ \u09B8\u09C1\u09B8\u0999\u09CD\u0997\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.439258-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2 \u098F\u09B0 \u09AD\u09C7\u09A4\u09B0\u09C7\u09B0 \u09B8\u09AE\u09B8\
  \u09CD\u09A4 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\
  \ \u09AA\u09CD\u09B0\u09A4\u09BF\u09B6\u09AC\u09CD\u09A6\u09C7 \u09AA\u09B0\u09BF\
  \u09A3\u09A4 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AE\u09C2\u09B2\u09A4\
  \ \u09B8\u09C1\u09B8\u0999\u09CD\u0997\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0995\u09B0\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কী এবং কেন?
একটি স্ট্রিংকে লোয়ারকেসে রূপান্তর করার অর্থ হল এর ভেতরের সমস্ত অক্ষরগুলোকে তাদের লোয়ারকেস প্রতিশব্দে পরিণত করা। প্রোগ্রামাররা এটি মূলত সুসঙ্গতির জন্য করে থাকেন, বিশেষ করে কেস-ইনসেন্সিটিভ তুলনাগুলিতে, যেমন ব্যবহারকারীর ইনপুটকে সামঞ্জস্য করা অথবা টেক্সট কনটেন্টে অনুসন্ধান করার সময়।

## কিভাবে:
জাভাস্ক্রিপ্টে, আমরা একটি স্ট্রিংকে `.toLowerCase()` মেথডের মাধ্যমে লোয়ারকেসে রূপান্তর করি। এটি খুবই সহজ:

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hello, world!"
```

ব্যবহারের সময়, মূল স্ট্রিংয়ের প্রতিটি অক্ষর সম্ভব হলে লোয়ারকেসে রূপান্তরিত হয়:

```javascript
let mixedCase = "jAvAScript ROCKs!";
let lowerCased = mixedCase.toLowerCase();
console.log(lowerCased); // "javascript rocks!"
```

লক্ষ্য করুন যে যেসব অক্ষরের লোয়ারকেস প্রতিশব্দ নেই সেগুলো অপরিবর্তিত থাকে।

## গভীর ডাইভ
পুরোনো দিনগুলিতে, টেক্সট পরিচালনা করার অর্থ ছিল চরিত্র এনকোডিং এবং ম্যানুয়াল রূপান্তরের প্রতি সতর্ক থাকা। কিন্তু আধুনিক জাভাস্ক্রিপ্টে, `.toLowerCase()` সেই জটিলতাগুলোকে আড়াল করে। এর আড়ালে, এটি অক্ষরগুলিকে রূপান্তর করার জন্য ইউনিকোড ম্যাপিংস ব্যবহার করে, তাই এটি A-Z এর চেয়ে বেশি কিছু নিয়ে কাজ করে।

বিকল্প পদ্ধতিগুলিও বিদ্যমান, যেমন:

- `toLocaleLowerCase()`: এটি ব্যবহারকারীর লোকেলকে সম্মান করে, যা কিছু ভাষার জন্য অপরিহার্য যেখানে লোয়ারকেসিং এর নিয়মগুলি প্রসঙ্গ-নির্দিষ্ট।

- নিয়মবিধিতের ব্যবহার: `toLowerCase()` এর আগে, ডেভেলপাররা হয়ত বড় হাতের অক্ষরগুলিকে ম্যানুয়ালি প্রতিস্থাপন করতে regex ব্যবহার করে থাকতে পারে।

বিস্তারিতভাবে, মনে রাখুন `.toLowerCase()` মূল স্ট্রিংকে পরিবর্তন করে না (JavaScript-এ স্ট্রিংগুলি অপরিবর্তনীয়)। আপনি সবসময় একটি নতুন স্ট্রিং পান। এটি ইউনিকোড স্ট্যান্ডার্ড দ্বারা বড় হাতের হিসেবে স্বীকৃত সমস্ত অক্ষরগুলিকে সম্ভালে, যার অর্থ আপনি বিভিন্ন ভাষা এবং লিপি জুড়ে সুরক্ষিত।

## দেখুন
- [MDN ওয়েব ডকসে toLowerCase সম্পর্কে](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [কেসিংয়ের জন্য ইউনিকোড স্ট্যান্ডার্ড](https://unicode.org/reports/tr21/tr21-5.html)
- [লোকেলের সাথে উপরে এবং নীচে কেস: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
