---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:19.418758-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \u09B2\u09C7 \u09A6\u09C1\u099F\u09BF \u09AC\u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\
  \u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09B8\u0995\u09C7 \u098F\u0995\
  \u09B8\u09BE\u09A5\u09C7 \u09B2\u09BE\u0997\u09BF\u09DF\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE \u09B9\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\
  \u09BE \u09A4\u09C8\u09B0\u09BF, \u09A1\u09BE\u0987\u09A8\u09BE\u09AE\u09BF\u0995\
  \ \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09A4\u09C8\u09B0\u09BF, \u0985\u09A5\
  \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0995\u09BF\u099B\u09C1\u2026"
lastmod: '2024-03-17T18:47:43.756397-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \u09B2\u09C7 \u09A6\u09C1\u099F\u09BF \u09AC\u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\
  \u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09B8\u0995\u09C7 \u098F\u0995\
  \u09B8\u09BE\u09A5\u09C7 \u09B2\u09BE\u0997\u09BF\u09DF\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE \u09B9\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\
  \u09BE \u09A4\u09C8\u09B0\u09BF, \u09A1\u09BE\u0987\u09A8\u09BE\u09AE\u09BF\u0995\
  \ \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09A4\u09C8\u09B0\u09BF, \u0985\u09A5\
  \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0995\u09BF\u099B\u09C1\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

স্ট্রিং জোড়ালে দুটি বা ততোধিক স্ট্রিংসকে একসাথে লাগিয়ে একটি স্ট্রিং তৈরি করা হয়। প্রোগ্রামাররা এটি বার্তা তৈরি, ডাইনামিক কনটেন্ট তৈরি, অথবা যেকোনো কিছু যেখানে লচ্ছিত উপায়ে পাঠ্যকে মিলানোর প্রয়োজন হয় সেখানে করে থাকেন।

## কিভাবে:

```TypeScript
let greeting: string = "Hello";
let target: string = "World";
let message: string = greeting + ", " + target + "!"; // যোগ (+) অপারেটর ব্যবহার করে
console.log(message); // আউটপুট: Hello, World!

let anotherMessage: string = `${greeting}, ${target}!`; // টেম্পলেট লিটারাল ব্যবহার করে
console.log(anotherMessage); // আউটপুট: Hello, World!
```

## গভীর ডুব

জোড়ালে মৌলিক; এটি প্রোগ্রামিংয়ের প্রাথমিক দিনগুলিতে থেকেছে। TypeScript এ, যা JavaScript এর উপর নির্মিত, আমরা জটিল স্ট্রিং অপারেশন থেকে টেম্পলেট লিটারালের মত চমকপ্রদ বৈশিষ্ট্যে এসেছি।

ঐতিহাসিকভাবে, আপনাকে জোড়া দেওয়ার ক্ষেত্রে সাবধান থাকতে হত, যাতে অতিরিক্ত মেমোরি ব্যবহার না হয় অথবা ব্রাউজার ধীর না হয়। আধুনিক ইঞ্জিন অপ্টিমাইজড, তবে বৃহৎ স্কেলের অ্যাপ্সে কার্যকারিতা এখনও গুরুত্বপূর্ণ।

বিকল্প পদ্ধতিগুলি রয়েছে:
1. অ্যারে এবং `.join()`: যখন আপনি স্ট্রিংগুলির একটি তালিকার সাথে ডিল করছেন।
2. StringBuilder প্যাটার্নস: জাভা বা C# এর মত ভাষাগুলিতে আরো প্রাসঙ্গিক, যেখানে এটি কর্মক্ষমতা অপ্টিমাইজ করে।

বাস্তবায়ন বিষয়ে, TypeScript শেষ পর্যন্ত JavaScript এ কম্পাইল হয়। অন্তরালে, এটি JavaScript দ্বারা প্রদত্ত একই স্ট্রিং ফাংশন এবং অপারেশনগুলি ব্যবহার করে।

## আরও দেখুন

- Mozilla Developer Network এর [String documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) দেখতে পারেন স্ট্রিং মেথডস সম্পর্কে গভীর ধারণা পেতে।
- TypeScript-বিষয়ক স্ট্রিং প্রশ্নের জন্য, [TypeScript's official documentation](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string) একটি দ্রুত রেফারেন্স।
