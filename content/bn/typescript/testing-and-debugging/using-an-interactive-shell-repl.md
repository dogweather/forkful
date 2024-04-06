---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:49.606344-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript \u09A8\u09BF\u099C\u09C7\
  \u09B0 \u0995\u09CB\u09A8\u09CB REPL \u09B8\u09B9 \u0986\u09B8\u09C7 \u09A8\u09BE\
  \u0964 `ts-node` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09AF\
  \u09BE\u0995, \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF TypeScript \u09A8\u09BF\
  \u09B0\u09CD\u09AC\u09BE\u09B9 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6 Node.js \u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09BE\u09A4\u09C7 \u098F\u0995\u099F\u09BF\
  \ REPL \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\u2026"
lastmod: '2024-04-05T21:53:51.911603-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u09A8\u09BF\u099C\u09C7\u09B0 \u0995\u09CB\u09A8\u09CB REPL\
  \ \u09B8\u09B9 \u0986\u09B8\u09C7 \u09A8\u09BE\u0964 `ts-node` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09AF\u09BE\u0995, \u098F\u099F\u09BF\
  \ \u098F\u0995\u099F\u09BF TypeScript \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\u09B9\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6 Node.js \u098F\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AF\u09BE\u09A4\u09C7 \u098F\u0995\u099F\u09BF REPL \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\
  \u09B0\u09A5\u09AE\u09C7, \u098F\u099F\u09BF \u0997\u09CD\u09B2\u09CB\u09AC\u09BE\
  \u09B2\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
TypeScript নিজের কোনো REPL সহ আসে না। `ts-node` ব্যবহার করা যাক, এটি একটি TypeScript নির্বাহ পরিবেশ Node.js এর জন্য যাতে একটি REPL অন্তর্ভুক্ত হয়।

প্রথমে, এটি গ্লোবালি ইনস্টল করুন:
```bash
npm install -g ts-node
```

REPL শুরু করতে আপনার কম্যান্ড লাইনে `ts-node` টাইপ করুন:
```bash
ts-node
```

এখানে একটি দ্রুত স্নিপেট চেষ্টা করতে পারেন:
```TypeScript
> let message: string = 'Hello, REPL!';
> console.log(message);
Hello, REPL!
> 
```
সেশন শেষ করতে, `Ctrl+D` চাপুন।

## গভীর ডাইভ
ঐতিহাসিকভাবে, REPLs লিস্প এর মতো ভাষায় প্রমিনেন্ট ছিল, যা ডাইনামিক কোড মূল্যায়নের জন্য অনুমোদন দেয়। এই ধারণাটি তারপর থেকে ছড়িয়ে পড়েছে, অনেক ভাষায় ইন্টারেক্টিভ কোডিংয়ের জন্য একটি স্ট্যাপল হয়ে উঠেছে।

TypeScript এর জন্য, `ts-node` আপনার একমাত্র বিকল্প নয়। বিকল্পগুলো অন্তর্ভুক্ত করে ওয়েব ব্রাউজারে টাইপস্ক্রিপ্ট প্লেগ্রাউন্ড ব্যবহার করা অথবা উপযুক্ত প্লাগইনগুলি সহ Node.js-ভিত্তিক অন্যান্য REPLs সমর্থন করা।

বাস্তবায়নের দিক থেকে, `ts-node` টাইপস্ক্রিপ্ট কম্পাইলার API ব্যবহার করে কোডটি নির্বাহের আগে তাৎক্ষণিকভাবে ট্রান্সপাইল করে। এটি আপনাকে সাথে সাথে ফিডব্যাক দেয় এবং বিশেষত সেটআপের ঝামেলা ছাড়াই TypeScript-এর সর্বশেষ বৈশিষ্ট্যগুলি চেষ্টা করার জন্য বিশেষভাবে উপযোগী।

মনে রাখবেন – যদিও একটি REPL দ্রুত পরীক্ষার জন্য দুর্দান্ত, এটি পারম্পরিক, পরীক্ষণযোগ্য এবং রক্ষণাবেক্ষণযোগ্য কোড লেখার বিকল্প নয়। এটি শিখতে এবং অন্বেষণের জন্য একটি টূল, সঠিক উন্নয়ন অনুশীলনের বিকল্প নয়।

## আরো দেখুন
- [টাইপস্ক্রিপ্ট অফিসিয়াল ওয়েবসাইট](https://www.typescriptlang.org/)
- [GitHub-এ ts-node](https://github.com/TypeStrong/ts-node)
- [Node.js REPL ডকুমেন্টেশন](https://nodejs.org/api/repl.html)
- [টাইপস্ক্রিপ্ট প্লেগ্রাউন্ড](https://www.typescriptlang.org/play)
