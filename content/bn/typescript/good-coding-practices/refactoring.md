---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:26.856488-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\
  \u09A8 \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1\
  \u09C7\u09B0 \u09AA\u09C1\u09A8\u0983\u09B0\u099A\u09A8\u09BE \u0995\u09B0\u09BE\
  , \u0985\u09A5\u099A \u098F\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\
  \u099A\u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09CB\u09A1\u0995\u09C7 \u0986\
  \u09B0\u0993 \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0, \u09B0\u0995\u09CD\
  \u09B7\u09A3\u09BE\u09AC\u09C7\u0995\u09CD\u09B7\u09A3\u09AF\u09CB\u0997\u09CD\u09AF\
  \u2026"
lastmod: '2024-03-17T18:47:43.773733-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\
  \u09A8 \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1\
  \u09C7\u09B0 \u09AA\u09C1\u09A8\u0983\u09B0\u099A\u09A8\u09BE \u0995\u09B0\u09BE\
  , \u0985\u09A5\u099A \u098F\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\
  \u099A\u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09CB\u09A1\u0995\u09C7 \u0986\
  \u09B0\u0993 \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0, \u09B0\u0995\u09CD\
  \u09B7\u09A3\u09BE\u09AC\u09C7\u0995\u09CD\u09B7\u09A3\u09AF\u09CB\u0997\u09CD\u09AF\
  \ \u098F\u09AC\u0982 \u099C\u099F\u09BF\u09B2\u09A4\u09BE \u09B9\u09CD\u09B0\u09BE\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09C7\u09A8, \u09AF\u09BE \u098F\u099F\u09BF\u0995\u09C7 \u09A8\u09A4\
  \u09C1\u09A8 \u0995\u09B0\u09C7 \u09A1\u09C1\u09AC \u09A6\u09C7\u0993\u09DF\u09BE\
  \ \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09C1\u099D\u09A4\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7\u0964."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:
একটি TypeScript ফাংশন বিবেচনা করুন যা আগের দিনগুলো থেকে ভালো ছিল - এটি একটু অগোছালো, এবং কিছু যত্ন ও মমতার প্রয়োজন:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
রিফ্যাক্টরিং করা হলে, এটি দেখতে এরকম হবে:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

দ্বিতীয় উদাহরণটি আরও শক্তিশালী, TypeScript-এর টাইপ সিস্টেমকে `interface`-এর মাধ্যমে ব্যবহার করে, সম্ভাব্য রানটাইম ত্রুটিগুলি এড়ানো এবং পঠনযোগ্যতা উন্নতি করে।

## গভীর অনুসন্ধান
রিফ্যাক্টরিং একটি আধুনিক ধারণা নয়; এটি প্রোগ্রামিং এর সাথে বিকশিত হয়েছে, ১৯৯৯ সালে মার্টিন ফাউলারের বই "Refactoring: Improving the Design of Existing Code" মুক্তি পাওয়ার সাথে সাথে আরও ঔপচারিক হয়ে উঠেছে। এটি একটি অজাইল ডেভেলপমেন্ট পরিবেশে অত্যন্ত জরুরী, অ্যাডাপ্টিভ কোড পরিবর্তনগুলিকে সহায়তা করে। ম্যানুয়াল রিফ্যাক্টরিংয়ের বিকল্পের মধ্যে TSLint বা TypeScript-এর নিজস্ব ভাষা সার্ভারের মতো অটোমেটেড যন্ত্রগুলি রয়েছে যা আপনার জন্য নির্দিষ্ট রিফ্যাক্টরিং কর্মকাণ্ডগুলি সুপারিশ বা এমনকি করতে পারে। বাস্তবায়নের বিস্তারিত সাধারণত "কোড গন্ধগুলি" চিনতে, যেমন ডুপ্লিকেট কোড, লম্বা পদ্ধতি, বা বড় ক্লাসগুলি চিনতে এবং রিফ্যাক্টরিংয়ের হাও এবং কেন বুঝতে প্যাটার্ন প্রয়োগ করা—যেমন পদ্ধতি বের করা, আরও উপযুক্ত ক্লাসগুলিতে সরানো, বা সরল কনস্ট্রাক্টগুলি ব্যবহার করা জড়িত। এই প্যাটার্নগুলি রিফ্যাক্টরিংয়ের হাও এবং কেন বুঝতে মূল বিষয়।

## আরও দেখুন
- [মার্টিন ফাউলারের বই "Refactoring: Improving the Design of Existing Code"](https://martinfowler.com/books/refactoring.html)
- [স্ট্যাটিক কোড বিশ্লেষণের জন্য TSLint](https://palantir.github.io/tslint/)
- [কোড গন্ধগুলি বোঝা](https://refactoring.guru/refactoring/smells)
