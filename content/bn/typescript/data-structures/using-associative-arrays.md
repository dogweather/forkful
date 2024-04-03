---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:14.643307-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099F\u09BE\u0987\u09AA\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u098F\u09B8\u09CB\u09B8\u09BF\
  \u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\u09AF\u09BE\u09B0\u09C7\u09B8\
  \ \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09B8\u09CB\u099C\u09BE\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u09A7\u09BE\u09AA\
  \u09C7 \u09A7\u09BE\u09AA\u09C7."
lastmod: '2024-03-17T18:47:43.757497-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09BE\u0987\u09AA\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD\
  \ \u0985\u09CD\u09AF\u09BE\u09B0\u09C7\u09B8 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\
  \u0982 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B8\u09CB\
  \u099C\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\
  \u09CC\u09B2\u09BF\u0995 \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7."
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কিভাবে:
টাইপস্ক্রিপ্টে এসোসিয়েটিভ অ্যারেস তৈরি এবং ব্যবহার করা সোজা। এখানে একটি মৌলিক ধাপে ধাপে:

```TypeScript
// একটি এসোসিয়েটিভ অ্যারে ঘোষণা
let user: { [key: string]: string } = {};

// ডাটা যোগ করা
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

আউটপুট:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

কী-মানের জুড়ি উপর ইতারেশন করা সহজ:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

আউটপুট:

```TypeScript
name: Jane Doe
email: jane@example.com
```

এবং যদি আপনি বিভিন্ন ধরনের ডাটা নিয়ে কাজ করছেন, টাইপস্ক্রিপ্টের টাইপ সিস্টেম উপযোগী:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

আউটপুট:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## গভীরে ডুব দেওয়া
টাইপস্ক্রিপ্টে, আমরা যা এসোসিয়েটিভ অ্যারেস বলে থাকি তা মূলত অবজেক্ট। ঐতিহাসিকভাবে, পিএইচপি এর মত ভাষায়, এসোসিয়েটিভ অ্যারেগুলি একটি মৌলিক প্রকার, কিন্তু জাভাস্ক্রিপ্ট (এবং সম্প্রসারণে, টাইপস্ক্রিপ্ট) এই উদ্দেশ্যে অবজেক্টের ব্যবহার করে। এই প্রভাবটি একটি শক্তি এবং সীমাবদ্ধতা উভয়ই। অবজেক্ট মানের সাথে স্ট্রিং এসোসিয়েট করার জন্য একটি অত্যন্ত ডায়নামিক কাঠামো প্রদান করে, কিন্তু ঐতিহ্যগত অর্থে 'অ্যারে' হিসাবে ব্যবহারের উদ্দেশ্যে নাও হতে পারে। উদাহরণস্বরূপ, আপনি সরাসরি এই অবজেক্টে `push` বা `pop` জাতীয় অ্যারে পদ্ধতি ব্যবহার করতে পারবেন না।

কী-মানের জোড়ের আদেশিক সংগ্রহের জন্য যেখানে অ্যারের মত অপারেশন প্রয়োজন, টাইপস্ক্রিপ্ট (এবং আধুনিক জাভাস্ক্রিপ্ট) `Map` অবজেক্ট অফার করে:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

যদিও টাইপস্ক্রিপ্টের টাইপ সিস্টেম এবং ES6 বৈশিষ্ট্য যেমন `Map` শক্তিশালী বিকল্প প্রদান করে, অবজেক্ট লিটারাল হিসাবে এসোসিয়েটিভ অ্যারেগুলি ব্যবহার করার উপায় বুঝতে পারা যেখানে অবজেক্ট লিটারাল বেশি কার্যকর বা JSON ডাটা কাঠামো নিয়ে কাজ করার সময় উপযোগী। এটি কাজের জন্য সঠিক টুল বাছাই করা সম্পর্কে।
