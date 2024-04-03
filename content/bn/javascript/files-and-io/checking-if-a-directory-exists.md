---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:31.809244-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Node.js \u098F, JavaScript \u09A8\
  \u09BF\u099C\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\
  \u0995\u09CD\u09B8\u09C7\u09B8 \u09A8\u09BE \u09A5\u09BE\u0995\u09BE\u09AF\u09BC\
  , \u098F \u09A7\u09B0\u09A8\u09C7\u09B0 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4\
  \ `fs` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2\u2026"
lastmod: '2024-03-17T18:47:44.467679-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u098F, JavaScript \u09A8\u09BF\u099C\u09C7 \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u09A8\u09BE\
  \ \u09A5\u09BE\u0995\u09BE\u09AF\u09BC, \u098F \u09A7\u09B0\u09A8\u09C7\u09B0 \u0985\
  \u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 `fs` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09AE\u09CD\u09AA\
  \u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2 `fs.existsSync()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09AF\u09C7\u09AD\u09BE\u09AC\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u099A\u09C7\
  \u0995 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
Node.js এ, JavaScript নিজে সরাসরি ফাইল সিস্টেমের অ্যাক্সেস না থাকায়, এ ধরনের অপারেশনের জন্য সাধারণত `fs` মডিউল ব্যবহার করা হয়। এখানে একটি সিম্পল উপায় দেওয়া হল `fs.existsSync()` ব্যবহার করে যেভাবে ডিরেক্টরি অস্তিত্ব চেক করা যায়:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// চেক করে দেখা হলো ডিরেক্টরি আছে কিনা
if (fs.existsSync(directoryPath)) {
  console.log('Directory exists.');
} else {
  console.log('Directory does not exist.');
}
```
**স্যাম্পল আউটপুট:**
```
Directory exists.
```
না-অবরোধকারী অসিঙ্ক্রোনাস পদ্ধতির জন্য, `fs.promises` এর সাথে `async/await` ব্যবহার করুন:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Directory exists.');
  } catch (error) {
    console.log('Directory does not exist.');
  }
}

checkDirectory('./sample-directory');
```
**স্যাম্পল আউটপুট:**
```
Directory exists.
```

যেসব প্রকল্পে ফাইল এবং ডিরেক্টরি অপারেশনের ভারী ব্যবহার রয়েছে, `fs-extra` প্যাকেজটি, নেটিভ `fs` মডিউলের একটি এক্সটেনশন, অতিরিক্ত সুবিধাজনক পদ্ধতি প্রদান করে। এখানে আপনি `fs-extra` দিয়ে একই কাজ কিভাবে অর্জন করতে পারেন তা দেখানো হলো:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// চেক করে দেখা হলো ডিরেক্টরি আছে কিনা
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Directory exists.' : 'Directory does not exist.'))
  .catch(err => console.error(err));
```
**স্যাম্পল আউটপুট:**
```
Directory exists.
```

এই পদ্ধতি আধুনিক JavaScript অনুশীলনের সাথে নির্বিঘ্নে একীভূত কোড প্রদান করে যা পরিষ্কার এবং পাঠযোগ্য।
