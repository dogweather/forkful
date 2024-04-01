---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:34.439856-06:00
description: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\
  \u09A8 \u09AC\u09BF\u09B6\u09C7\u09B7\u09BE\u09AF\u09BC\u09BF\u09A4 \u099F\u09C1\
  \u09B2\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09C1\u0995\u09CD\u09A4\
  \ \u09B9\u0993\u09AF\u09BC\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0985\u09AD\u09CD\
  \u09AF\u09A8\u09CD\u09A4\u09B0\u09C7 \u099A\u09CB\u0996 \u09B0\u09BE\u0996\u09A4\
  \u09C7 \u09A6\u09C7\u09AF\u09BC, \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7\
  \ \u098F\u099F\u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u099A\u09B2\u09C7 \u09A4\
  \u09BE \u09A6\u09C7\u0996\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\u2026"
lastmod: '2024-03-17T18:47:44.457838-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\u09A8\
  \ \u09AC\u09BF\u09B6\u09C7\u09B7\u09BE\u09AF\u09BC\u09BF\u09A4 \u099F\u09C1\u09B2\
  \u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09C1\u0995\u09CD\u09A4 \u09B9\
  \u0993\u09AF\u09BC\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0985\u09AD\u09CD\u09AF\
  \u09A8\u09CD\u09A4\u09B0\u09C7 \u099A\u09CB\u0996 \u09B0\u09BE\u0996\u09A4\u09C7\
  \ \u09A6\u09C7\u09AF\u09BC, \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7 \u098F\
  \u099F\u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u099A\u09B2\u09C7 \u09A4\u09BE\
  \ \u09A6\u09C7\u0996\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
---

## কিভাবে:
এখানে এমন কিছু JavaScript কোড দেওয়া হল যা প্রত্যাশিতভাবে আচরণ করছে না:

```javascript
function buggyMultiply(a, b) {
    return a + b; // ওহো! এটা উচিত ছিল গুণ, যোগ নয়।
}

let result = buggyMultiply(5, 3);
console.log('ফলাফল:', result);
```

আউটপুট ভুল হয়ে গেছে:
```
ফলাফল: 8
```

চলুন Chrome DevTools এ ডিবাগ করি:

1. এই JS টি একটি ব্রাউজারে খুলুন।
2. ডান ক্লিক করে "Inspect" নির্বাচন করে DevTools খুলুন।
3. "Sources" ট্যাবে ক্লিক করুন।
4. আপনার কোড স্নিপেট বা পৃষ্ঠা খুঁজে বের করুন এবং `return` স্টেটমেন্টের পাশের লাইন নম্বরে ক্লিক করে একটি ব্রেকপয়েন্ট সেট করুন।
5. ব্রেকপয়েন্ট সক্রিয় করতে পৃষ্ঠাটি রিফ্রেশ করুন।
6. লোকাল ভেরিয়েবলগুলি `a` এবং `b` দেখার জন্য "Scope" প্যানেলটি চেক করুন।
7. "Step over next function call" বাটন ব্যবহার করে পরবর্তী ধাপে যান।
8. `return` স্টেটমেন্টে বাগটি খুঁজে বের করুন।
9. কোড ঠিক করুন:
```javascript
function buggyMultiply(a, b) {
    return a * b; // ঠিক করা হয়েছে!
}

let result = buggyMultiply(5, 3);
console.log('ফলাফল:', result);
```

সংশোধিত আউটপুট:
```
ফলাফল: 15
```

## গভীর ডুব
ডিবাগিং এর ধারণা কম্পিউটিং এর প্রাথমিক দিনগুলিতে থেকেই ছিল—কিংবদন্তি বলে, এটি ১৯৪০ এর দশকে একটি কম্পিউটারে মথ পাওয়ার পর থেকে শুরু হয়েছিল! আজকে, JavaScript ডিবাগার যেমন ব্রাউজারের অন্তর্ভুক্ত টুলস (Chrome DevTools, Firefox Developer Tools) বা IDE এর সাথে ইন্টিগ্রেটেড ডিবাগার (Visual Studio Code, WebStorm) অনেক ধরণের বৈশিষ্ট্য অফার করে।

অন্তর্নির্মিত ডিবাগারের বিকল্পের মধ্যে রয়েছে থার্ড-পার্টি টুলস যেমন WebStorm বা ভেরিয়েবলের স্টেট আউটপুট করার জন্য সাধারণ `console.log` ব্যবহার করা। কিন্তু এগুলি ডিবাগার প্রদান করা রিয়েল-টাইম ইন্টারেকশন এবং বিস্তারিত পরীক্ষা অফার করে না।

বাস্তবায়নের বিস্তারিত, বেশিরভাগ ডিবাগার অনুরূপভাবে কাজ করে: এগুলি আপনাকে এমন ব্রেকপয়েন্ট সেট করতে দেয় যা একজিকিউশন থামিয়ে দেয়, কোড ধাপে ধাপে পরীক্ষা করতে দেয়, বর্তমান ভেরিয়েবল স্টেট পরীক্ষা করতে দেয়, এক্সপ্রেশন ওয়াচ করতে দেয়, এবং ভিন্ন সেনারিও পরীক্ষা করার জন্য উড়ন্ত মান পরিবর্তন করতে দেয়।

## আরো দেখুন
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - ডিবাগিং](https://code.visualstudio.com/docs/editor/debugging)
