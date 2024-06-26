---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:49.324107-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7, \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09B8 \u09B8\u09AE\u09CD\u09AE\
  \u09BF\u09B2\u09A8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09DF\
  \u09C7\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09DF \u09B0\u09DF\u09C7\u099B\u09C7\
  \u0964 \u09AA\u09C1\u09B0\u09A8\u09CB \u09B8\u09CD\u0995\u09C1\u09B2: `+`\u0964\
  \ \u0986\u09A7\u09C1\u09A8\u09BF\u0995: \u099F\u09C7\u09AE\u09AA\u09CD\u09B2\u09C7\
  \u099F \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\u09B2\u09B8\u0964 \u098F\u09B0\u09BE\
  \ \u0995\u09C7\u09AE\u09A8 \u09A6\u09C7\u0996\u09BE\u09DF \u09A4\u09BE \u098F\u0996\
  \u09BE\u09A8\u09C7\u0964 **`+`\u2026"
lastmod: '2024-03-17T18:47:44.444660-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u09B8 \u09B8\u09AE\u09CD\u09AE\u09BF\u09B2\u09A8 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09DF\u09C7\u0995\u099F\u09BF \u0989\u09AA\u09BE\
  \u09DF \u09B0\u09DF\u09C7\u099B\u09C7\u0964 \u09AA\u09C1\u09B0\u09A8\u09CB \u09B8\
  \u09CD\u0995\u09C1\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
জাভাস্ক্রিপ্টে, আপনার স্ট্রিংস সম্মিলন করার জন্য কয়েকটি উপায় রয়েছে। পুরনো স্কুল: `+`। আধুনিক: টেমপ্লেট লিটারালস। এরা কেমন দেখায় তা এখানে।

**`+` অপারেটর ব্যবহার করে:**
```javascript
let hello = "Hello, ";
let world = "world!";
let greeting = hello + world; 
console.log(greeting); // "Hello, world!"
```

**টেমপ্লেট লিটারালস ব্যবহার করে:**
```javascript
let user = "Jane";
let welcomeMessage = `Hi, ${user}! Welcome back.`;
console.log(welcomeMessage); // "Hi, Jane! Welcome back."
```

## গভীর ডুব
পুরানো দিনে, `+` ছিল যেতে হবের পথ, কিন্তু অনেক ভেরিয়াবলের সাথে এটা জটিল হয়ে যেত। ২০১৫ এ ES6 প্রবর্তন করে, টেমপ্লেট লিটারালস (সেই ব্যাকটিকগুলো `\``) নিয়ে আসে। এর মানে ছিল, আরও পরিষ্কার দেখানো স্ট্রিংস এবং আপনার স্ট্রিংয়ের মধ্যে সরাসরি ভেরিয়াবল এবং প্রকাশগুলি নিক্ষেপ করার ক্ষমতা যা ঘামের ছিটে ফেলা ছাড়াই সম্ভব।

**কেন `+` বিরক্তিকর হতে পারে:**
- একাধিক ভেরিয়াবলের সাথে কঠিন পাঠযোগ্যতা।
- স্থান মিস করে ফেলা সহজ, যা শব্দগুলিকে চেপে যেতে পারে।
- প্লাস, সব এই প্লাসগুলির প্রয়োজন কে আছে?

**কেন টেমপ্লেট লিটারালস চমৎকার:**
- পড়ার সহজতা: খালি স্থানগুলি পূরণ করে একটি ইংরেজি বাক্যের মতো।
- মাল্টিলাইন সাপোর্ট: আপনি `+` বা `\n` ছাড়াই একাধিক লাইনের স্ট্রিং তৈরি করতে পারেন।
- প্রকাশ ইন্টারপোলেশন: একেবারে ভেরিয়াবলগুলি পপ ইন করুন, গণিত করুন, সব এক যাত্রায়।

**মাল্টিলাইন এবং প্রকাশের কর্মে এখানে:**
```javascript
let apples = 3;
let oranges = 5;
let fruitSummary = `You have ${apples + oranges} pieces of fruit: 
${apples} apples and 
${oranges} oranges.`;
console.log(fruitSummary);
```
কোনো `+` অ্যাক্রোব্যাটিক্স ছাড়াই একটি পরিপাটি সংক্ষিপ্ত বিবরণ আউটপুট।

প্রযুক্তিগতভাবে, স্ট্রিং সম্মিলন প্রতিবার `+` ব্যবহার করে একটি নতুন স্ট্রিং তৈরি করে। কম্পিউটারের জন্য, এটা যেন আরেকটি মটের যোগ করতে চাইলে পুরো নতুন চকলেট বার তৈরি করার মতো। খুবই কার্যকর নয়। টেমপ্লেট লিটারালস হচ্ছে যেন একটি ছাঁচ যেখানে আপনি একবারে সমস্ত উপাদান ফেলতে পারেন - বড় স্ট্রিং বা লুপগুলিতে বিশেষত ভালো কর্মক্ষমতা, বিশেষ করে।

## আরও দেখুন
- টেমপ্লেট লিটারালস সম্পর্কে MDN ওয়েব ডক্স (আরও পড়ার জন্য): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- স্ট্রিং পদ্ধতি এবং বৈশিষ্ট্যাবলী (স্ট্রিংস সম্পর্কে কাজ করার সময় উপকারী): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
