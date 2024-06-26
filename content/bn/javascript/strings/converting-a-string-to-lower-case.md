---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:43.532957-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7, \u0986\u09AE\u09B0\u09BE\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7\
  \ `.toLowerCase()` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BF\u0964 \u098F\u099F\
  \u09BF \u0996\u09C1\u09AC\u0987 \u09B8\u09B9\u099C."
lastmod: '2024-03-17T18:47:44.439258-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7, \u0986\u09AE\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0995\u09C7 `.toLowerCase()` \u09AE\u09C7\u09A5\u09A1\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0\
  \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09BF\u0964 \u098F\u099F\u09BF \u0996\u09C1\u09AC\u0987 \u09B8\u09B9\u099C\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

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
