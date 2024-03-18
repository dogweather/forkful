---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:26:58.075841-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

জাভাস্ক্রিপ্টে নিয়মিত অভিব্যক্তিসমূহ (regex) হলো প্যাটার্ন যা স্ট্রিংগুলিতে অক্ষরের সংমিশ্রণ মেলে ধরার জন্য ব্যবহৃত হয়। প্রোগ্রামাররা তাদের সন্ধান, এক্সট্রাকশন এবং টেক্সট ম্যানিপুলেশনের জন্য ব্যবহার করে, যা সংক্ষিপ্ত কোড দ্বারা শক্তিশালী স্ট্রিং প্রসেসিং অপারেশন সম্ভব করে তোলে।

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
