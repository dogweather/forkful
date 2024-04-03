---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:58.975236-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7, \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\
  \u09A8 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u099F\u09C7\u09AE\u09AA\u09CD\
  \u09B2\u09C7\u099F \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\u09B2\u09B8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09B0\u09BE \u09B9\u09DF\
  \u0964 \u098F\u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09AF\u09BC \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0996\
  \u09BE\u09A8\u09CB \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.438274-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\
  \u0987 \u099F\u09C7\u09AE\u09AA\u09CD\u09B2\u09C7\u099F \u09B2\u09BF\u099F\u09BE\
  \u09B0\u09BE\u09B2\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u0995\u09B0\u09BE \u09B9\u09DF\u0964 \u098F\u099F\u09BF \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AF\u09BC \u09A4\u09BE \u09A8\
  \u09BF\u099A\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
জাভাস্ক্রিপ্টে, স্ট্রিং ইন্টারপোলেশন প্রায়ই টেমপ্লেট লিটারালস ব্যবহার করে করা হয়। এটি কিভাবে করতে হয় তা নিচে দেখানো হল:

```javascript
const name = 'Alice';
const message = `হ্যালো, ${name}! আজ তুমি কেমন আছো?`;
console.log(message); // আউটপুট: হ্যালো, Alice! আজ তুমি কেমন আছো?
```

আপনি প্লেসহোল্ডারের মধ্যে অপারেশনও সম্পাদন করতে পারেন:

```javascript
const a = 10;
const b = 5;
console.log(`দশ গুণ পাঁচ হল ${a * b}.`); // আউটপুট: দশ গুণ পাঁচ হল 50.
```

## গভীরে ডুব
ঐতিহাসিকভাবে, জাভাস্ক্রিপ্টে স্ট্রিং ইন্টারপোলেশন এত সরল ছিল না। ES6 (ECMAScript 2015) আসার পূর্বে, `+` অপারেটর ব্যবহার করে সংযুক্তি সাধারণত করা হত:

```javascript
var name = 'Bob';
var message = 'হ্যালো, ' + name + '! আজ তুমি কেমন আছো?';
```

ES6 এর সাথে আসা টেমপ্লেট লিটারালস (ব্যাকটিক্সের মধ্যে  ` ` `) সাথে আরো সহজ সিনট্যাক্স এবং `${}` প্লেসহোল্ডার সম্পর্কে ধারণা আনা হল।

স্ট্রিং ইন্টারপোলেশনের বিকল্পগুলি হলো স্ট্রিংকে `+` অপারেটর এবং `concat()` পদ্ধতির মাধ্যমে জোড়া লাগানো, অথবা তৃতীয়-পক্ষের লাইব্রেরিগুলি থেকে `sprintf`-এর মতো ফাংশন ব্যবহার করা।

টেমপ্লেট লিটারালসের পারফরম্যান্স সাধারণত এই পুরানো পদ্ধতিগুলির সাথে সমান থাকে। তবে, পড়ার সহজতা এবং স্ট্রিংগুলির মধ্যে প্রকাশ (যেমন `${a * b}`) যোগ করার ক্ষমতা টেমপ্লেট লিটারালসকে ডেভেলপারদের জন্য শক্তিশালী পছন্দ করে তুলেছে।

## আরও দেখুন
- টেমপ্লেট লিটারালস সম্পর্কে MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- জাভাস্ক্রিপ্ট স্ট্রিং সংযোজন: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators
- জাভাস্ক্রিপ্ট মডিউল "ECMAScript" এর ইতিহাস: https://www.ecma-international.org/publications-and-standards/standards/ecma-262/
