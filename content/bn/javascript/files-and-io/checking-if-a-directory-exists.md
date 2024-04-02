---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:31.809244-06:00
description: "JavaScript \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE \u09AB\u09BE\u0987\
  \u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\
  \ \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\
  \u09B9\u09BE\u09B0\u09CD\u09AF, \u09AF\u09BE \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF \u09AA\u09A1\u09BC\u09BE \u09AC\u09BE \u09A4\u09BE\u09A4\
  \u09C7 \u09B2\u09C7\u0996\u09BE \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0\
  \ \u0986\u0997\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.467679-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE \u09AB\u09BE\u0987\
  \u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\
  \ \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\
  \u09B9\u09BE\u09B0\u09CD\u09AF, \u09AF\u09BE \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF \u09AA\u09A1\u09BC\u09BE \u09AC\u09BE \u09A4\u09BE\u09A4\
  \u09C7 \u09B2\u09C7\u0996\u09BE \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0\
  \ \u0986\u0997\u09C7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কি এবং কেন?
JavaScript এ একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা ফাইল ম্যানিপুলেশন কাজের জন্য অপরিহার্য, যা স্ক্রিপ্টগুলিকে ডিরেক্টরি পড়া বা তাতে লেখা শুরু করার আগে ডিরেক্টরির উপস্থিতি যাচাই করতে সক্ষম করে। এই অপারেশন ত্রুটি প্রতিরোধ করে এবং বিশেষত যে সব অ্যাপ্লিকেশন ব্যবহারকারীর ইনপুট বা বাহ্যিক ডাটা সোর্স অনুযায়ী ডাইনামিকালি ফাইল বা ডিরেক্টরি সম্পর্কে অপারেশন সম্পাদন করে, তাদের জন্য মসৃণ প্রোগ্রামের এক্সিকিউশন নিশ্চিত করে।

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
