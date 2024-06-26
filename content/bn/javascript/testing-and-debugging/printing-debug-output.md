---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:48.208294-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F `console.log()` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\
  \u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\
  \u09B0\u09BE \u0996\u09C1\u09AC\u0987 \u09B8\u09B9\u099C\u0964 \u098F\u0987\u09AD\
  \u09BE\u09AC\u09C7."
lastmod: '2024-03-17T18:47:44.455846-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F `console.log()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\
  \u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u0996\u09C1\u09AC\u0987 \u09B8\
  \u09B9\u099C\u0964 \u098F\u0987\u09AD\u09BE\u09AC\u09C7."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
জাভাস্ক্রিপ্ট `console.log()` ব্যবহার করে ডিবাগ আউটপুট প্রিন্ট করা খুবই সহজ। এইভাবে:

```javascript
console.log('হ্যালো, ডিবাগ দুনিয়া!');

let number = 42;
console.log('সংখ্যাটি হল:', number);

function add(a, b) {
  console.log(`যোগ করা হচ্ছে ${a} + ${b}`);
  return a + b;
}

let result = add(3, 4);
console.log('ফলাফল:', result);
```

আপনার ব্রাউজারের কনসোল অথবা নোড.জেএস টার্মিনালে স্যাম্পল আউটপুট এর মত দেখাবে:

```
হ্যালো, ডিবাগ দুনিয়া!
সংখ্যাটি হল: 42
যোগ করা হচ্ছে 3 + 4
ফলাফল: 7
```

## গভীরে যাওয়া
`console.log()` মেথডটি কনসোল API থেকে আসে, যা ব্রাউজার এবং নোড.জেএস পরিবেশে যুগ যুগ ধরে ডিবাগিং বন্ধু হিসেবে পরিচিত। কিন্তু শুধুমাত্র `log` নয়; আপনার কাছে `console.warn()`, `console.error()`, এবং `console.info()` আছে, যা বিভিন্ন গুরুত্বের মেসেজ দেখায়।

অনেক আগে, ডেভেলপাররা ডিবাগিংয়ের জন্য `alert()` ব্যবহার করত, কিন্তু এটি দ্রুত বিরক্তিকর হয়ে উঠত—এটি একটি ডায়লগ বক্স পপ আপ করে ব্যবহারকারীর ইন্টারেকশন ব্লক করে দেয়।

এছাড়াও, একটি অবজেক্টের JSON-এর মত দর্শন দেয় এমন `console.dir()` আছে, যা গভীর পরীক্ষা নিরীক্ষার জন্য সুবিধাজনক। আপনি যদি কোনো কিছু করতে কত সময় লাগে তা ট্র্যাক করতে চান, `console.time()` এবং `console.timeEnd()` আপনার বন্ধু।

যারা একটি ভালো, পরিষ্কার আউটপুট পছন্দ করে তাদের জন্য, `console.table()` ডেটা প্রদর্শন করে একটি পরিপাটি টেবিল বিন্যাসে। এবং যখন আপনি সরল ডিবাগিং ছাড়িয়ে পারফর্ম্যান্সের দিকে যান, তখন কনসোল API-এর আরও টুলস আছে যেমন `console.trace()` কল স্ট্যাক তথ্যের জন্য, `console.profile()` পারফর্ম্যান্স প্রোফাইলিংয়ের জন্য, অন্যান্য মধ্যে।

`console` মেথডগুলির নির্দিষ্ট বাস্তবায়ন জাভাস্ক্রিপ্ট পরিবেশে ভিন্ন হতে পারে, কিন্তু মূল বিষয় একই থাকে: তারা ডেভেলপারদের কোডের আড়ালে কি ঘটছে তা দ্রুত এবং সহজে বুঝতে সাহায্য করে।

## আরও দেখুন
- কনসোল API এর উপর MDN ওয়েব ডকস: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Node.js `console` ডকুমেন্টেশন: https://nodejs.org/api/console.html
- কনসোল কমান্ডের গাইড: https://getfirebug.com/wiki/index.php/Console_API
