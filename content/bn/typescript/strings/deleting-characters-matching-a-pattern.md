---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:35.465381-06:00
description: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2 \u09B0\u09C7\u0996\u09C7 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\
  \u09B0\u09CD\u09A5 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u09A8\
  \u09C1\u0995\u09CD\u09B0\u09AE\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\
  \u09B2\u09BF (\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8)\u2026"
lastmod: '2024-03-17T18:47:43.747664-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2 \u09B0\u09C7\u0996\u09C7 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\
  \u09B0\u09CD\u09A5 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u09A8\
  \u09C1\u0995\u09CD\u09B0\u09AE\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\
  \u09B2\u09BF (\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8)\u2026"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কি এবং কেন?

একটি নির্দিষ্ট প্যাটার্নের সাথে মিল রেখে অক্ষরগুলি মুছে ফেলার প্রক্রিয়া অর্থ হল একটি স্ট্রিংয়ের মধ্যে একটি নির্দিষ্ট অনুক্রমের অক্ষরগুলি (প্যাটার্ন) অনুসন্ধান করা এবং তাদের মুছে ফেলা। প্রোগ্রামাররা টেক্সট ডেটা পরিষ্কার বা রূপান্তর করার জন্য এটি করে – চিন্তা করুন একটি স্ট্রিং থেকে HTML ট্যাগগুলি সরিয়ে ফেলা, অথবা অবাঞ্ছিত বিরামচিহ্ন মুছে ফেলা।

## কিভাবে:

```TypeScript
function deletePattern(text: string, pattern: string): string {
  // প্যাটার্ন স্ট্রিং থেকে একটি RegExp তৈরি করুন
  const regex = new RegExp(pattern, 'g');
  // প্যাটার্নের মিল থাকা অংশগুলিকে একটি খালি স্ট্রিং দিয়ে প্রতিস্থাপন করুন
  return text.replace(regex, '');
}

// উদাহরণ
const originalText = "Hello, World! This -- is a test.";
const newText = deletePattern(originalText, "[,\\-!]");
console.log(newText);  // আউটপুট: "Hello World This  is a test"
```

## গভীরে গিয়ে

ঐতিহাসিকভাবে, প্রোগ্রামিংয়ে স্ট্রিং নিয়ে কাজ করার অভিজ্ঞতা কম্পিউটিংয়ের প্রথমদিকের থেকেই ট্রেস করা যায়। TypeScript-এ, যা JavaScript-এর উপরে নির্মিত, স্ট্রিংগুলিকে প্রতিদিনের কাজে ম্যানিপুলেট করা হয়।  `replace()` ফাংশনটি আমরা যা ব্যবহার করেছি তা JavaScript-এর শক্তিশালী স্ট্রিং ম্যানিপুলেশন আর্সেনালের থেকে উত্তরাধিকারস্বরূপ পাওয়া।

RegExp এর বিকল্প হিসেবে প্যাটার্নগুলি ম্যাচ করার জন্য – কখনও কখনও আপনি প্রতিটি অক্ষর ম্যানুয়াল ইটারেট করা এবং একটি সুইচ স্টেটমেন্ট অথবা একাধিক ইফস্‌ এর মাধ্যমে সিদ্ধান্ত নেওয়ার কথা ভাবতে চাইতে পারেন। কিন্তু নিয়মিত এক্সপ্রেশনগুলি একটি সংক্ষিপ্ত এবং শক্তিশালী উপায় প্রদান করে জটিল প্যাটার্নগুলি ম্যাচ করার জন্য বর্ণনা করার।

যখন আপনি RegExp প্যাটার্নগুলি মানমিতি সময়ে কীভাবে ব্যাখ্যা করা হয় তার দিকে গভীরে ডুব দেন, তখন বাস্তবায়নের বিস্তারিত আকর্ষণীয় হয়ে ওঠে। RegExp নির্মাতায় 'g' ফ্ল্যাগটি ইঞ্জিনকে স্ট্রিং জুড়ে বিশ্বব্যাপী অনুসন্ধান করতে বলে। এটি ছাড়া, কেবল প্রথম মিলটি প্রতিস্থাপিত হতো। নিয়মিত এক্সপ্রেশনগুলি আপনার প্রয়োজনের উপর ভিত্তি করে সহজ বা অত্যন্ত জটিল হতে পারে।

## আরও দেখুন

- MDN ওয়েব ডকস্‌ রেগেক্স প্রসঙ্গে: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- TypeScript হ্যান্ডবুক স্ট্রিং ম্যানিপুলেশন প্রসঙ্গে: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- প্যাটার্ন তৈরির সাহায্যের জন্য নিয়মিত এক্সপ্রেশন টেস্টার: [https://regexr.com/](https://regexr.com/)
