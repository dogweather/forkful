---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:58.075841-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B6\u09C1\u09B0\u09C1 \u0995\
  \u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 regex \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \ \u098F\u09AC\u0982 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u09AF\u09BC\u09C7 \u09AE\u09C7\u09B2 \u0996\u09CB\u0981\u099C\u09BE\u09B0\
  \ \u099A\u09C7\u09B7\u09CD\u099F\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7, \u0986\u09AE\u09B0\u09BE \"code\"\
  \u2026"
lastmod: '2024-03-17T18:47:44.442669-06:00'
model: gpt-4-0125-preview
summary: "\u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 regex \u09AA\u09CD\
  \u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u098F\u09AC\u0982 \u098F\u099F\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09AE\u09C7\
  \u09B2 \u0996\u09CB\u0981\u099C\u09BE\u09B0 \u099A\u09C7\u09B7\u09CD\u099F\u09BE\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7, \u0986\u09AE\u09B0\u09BE \"code\" \u09B6\u09AC\u09CD\u09A6\u099F\u09BF\
  \ \u0996\u09C1\u0981\u099C\u09AC."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:


### বেসিক ম্যাচিং
শুরু করতে, আপনি একটি সাধারণ regex প্যাটার্ন তৈরি করতে পারেন এবং এটি ব্যবহার করে একটি স্ট্রিংয়ে মেল খোঁজার চেষ্টা করতে পারেন। এখানে, আমরা "code" শব্দটি খুঁজব:

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### `String.prototype.match()` ব্যবহার
ম্যাচগুলির একটি অ্যারে পেতে:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### গ্লোবাল সার্চ
সব ম্যাচ খুঁজতে, `g` ফ্ল্যাগ ব্যবহার করুন:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### কেস-অনুবেদনহীন ম্যাচিং
`i` ফ্ল্যাগ কেসকে উপেক্ষা করে:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### টেক্সট প্রতিস্থাপন
`String.prototype.replace()` ব্যবহার করে স্ট্রিংয়ের অংশ প্রতিস্থাপন করুন:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### গ্রুপ ব্যবহার
গ্রুপ প্যাটার্নের অংশ ক্যাপচার করতে পারে:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### থার্ড-পার্টি লাইব্রেরিগুলি
যদিও জাভাস্ক্রিপ্টের বিল্ট-ইন regex ক্ষমতা শক্তিশালী, কিছু কাজ যেমন `XRegExp` মতো লাইব্রেরিগুলি দ্বারা সরলীকৃত হতে পারে। এটি অতিরিক্ত সিন্ট্যাক্স এবং ফ্ল্যাগ প্রস্তাব করে, জটিল প্যাটার্নগুলিকে আরও পাঠযোগ্য করে তোলে:

```javascript
// XRegExp লাইব্রেরির উদাহরণ
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

এই স্নিপেটটি দেখায় কিভাবে `XRegExp` ব্যবহার করে একটি স্ট্রিংয়ে সমস্ত ইউনিকোড শব্দগুলি মেলে, যা জাভাস্ক্রিপ্টের বিল্ট-ইন ক্ষমতাগুলির বাইরে প্রসারিত অক্ষরের সেট সামলানোর লাইব্রেরির ক্ষমতা প্রদর্শন করে।
