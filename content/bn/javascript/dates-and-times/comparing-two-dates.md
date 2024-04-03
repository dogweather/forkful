---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:33.867595-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: JavaScript- \u098F\u09B0 `Date`\
  \ \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u098F\u0995\
  \u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u09C7 \u0996\u09C1\u09AC \u0989\u09AA\u0995\
  \u09BE\u09B0\u09BF\u0964 \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09AC\u09C7\u09A8\
  , \u09A4\u09BE\u09B0\u09BE \u09E7\u09EF\u09ED\u09E6 \u09B8\u09BE\u09B2\u09C7\u09B0\
  \ \u099C\u09BE\u09A8\u09C1\u09DF\u09BE\u09B0\u09C0 \u09E7 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A5\u09C7\u0995\u09C7 \u09AE\u09BF\u09B2\u09BF\u09B8\u09C7\u0995\u09C7\
  \u09A8\u09CD\u09A1\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u09BF\u09A4\
  \u2026"
lastmod: '2024-03-17T18:47:44.465710-06:00'
model: gpt-4-0125-preview
summary: "JavaScript- \u098F\u09B0 `Date` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \u0997\u09C1\u09B2\u09BF \u098F\u0995\u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u09C7\
  \ \u0996\u09C1\u09AC \u0989\u09AA\u0995\u09BE\u09B0\u09BF\u0964 \u09AF\u0996\u09A8\
  \ \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\u09A6\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\
  \u09BE \u0995\u09B0\u09AC\u09C7\u09A8, \u09A4\u09BE\u09B0\u09BE \u09E7\u09EF\u09ED\
  \u09E6 \u09B8\u09BE\u09B2\u09C7\u09B0 \u099C\u09BE\u09A8\u09C1\u09DF\u09BE\u09B0\
  \u09C0 \u09E7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A5\u09C7\u0995\u09C7 \u09AE\u09BF\
  \u09B2\u09BF\u09B8\u09C7\u0995\u09C7\u09A8\u09CD\u09A1\u09C7 \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0\u09BF\u09A4 \u09B9\u09AF\u09BC\u0964."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
JavaScript- এর `Date` অবজেক্টগুলি এক্ষেত্রে খুব উপকারি। যখন আপনি তাদের তুলনা করবেন, তারা ১৯৭০ সালের জানুয়ারী ১ তারিখ থেকে মিলিসেকেন্ডে রূপান্তরিত হয়।

```javascript
let date1 = new Date('2021-07-24');
let date2 = new Date('2021-07-25');

console.log(date1 < date2); // true
console.log(date1 > date2); // false
console.log(date1.getTime() === date2.getTime()); // false
```

নমুনা ফলাফল:

```
true
false
false
```

## গভীরে যান
অন্তর্নিহিতভাবে, `Date` অবজেক্টগুলি কেবল মিলিসেকেন্ড। ঐতিহাসিকভাবে, প্রোগ্রামারদের ম্যানুয়ালি তারিখ অপারেশনসমূহ পরিচালনা করতে হতো, একটি ডেটাম-পয়েন্ট থেকে সময় অতিক্রমের হিসেব করে, প্রায়ই ভুলের ঝুঁকি নিয়ে। `Date` অবজেক্টগুলির তুলনা করা জীবনকে সহজ করে দেয়, যদিও এখনও এরর-এর সম্ভাবনা আছে, বিশেষ করে সময় অঞ্চল এবং দিবালোক সাশ্রয়ী সময়ের বিষয়ে।

বিকল্প আছে? অবশ্যই। জটিল পরিস্থিতিগুলি সামলাতে এবং তারিখ নিপুণ করার জন্য অতিরিক্ত সুবিধা প্রদানের জন্য `moment.js` বা `date-fns` এর মতো লাইব্রেরিগুলি সাহায্য করে।

বাস্তবায়ন দিক থেকে, মুখ্য বিষয় হল সরাসরি `Date` অবজেক্টগুলি (যেমন `==` দ্বারা) তুলনা করলে তা তাদের রেফারেন্সকে তুলনা করে, মান নয়। সঠিক মানের তুলনা করতে `getTime()` ব্যবহার করুন। এবং তারিখ বিশ্লেষণ করার সময় সময় অঞ্চলের জন্য সতর্ক থাকুন; যদি আপনি সাবধান না হন তাহলে বিভ্রান্ত হওয়া সহজ।

## আরও দেখুন
- MDN ওয়েব ডকস তারিখে: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js লাইব্রেরি: https://momentjs.com/
- date-fns লাইব্রেরি: https://date-fns.org/
