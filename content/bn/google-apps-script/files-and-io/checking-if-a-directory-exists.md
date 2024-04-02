---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:15.365262-06:00
description: "Google Apps Script \u098F \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\
  \u09C7\u0995\u09CD\u099F\u09B0\u09BF \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0986\u099B\u09C7 \u0995\u09BF \u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 Google Drive \u098F\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u099F\u09BF \u09AB\u09CB\u09B2\u09CD\u09A1\
  \u09BE\u09B0\u09C7\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4\u2026"
lastmod: '2024-03-17T18:47:43.541520-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\
  \u0995\u09CD\u099F\u09B0\u09BF \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0986\u099B\u09C7 \u0995\u09BF \u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 Google Drive \u098F\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u099F\u09BF \u09AB\u09CB\u09B2\u09CD\u09A1\
  \u09BE\u09B0\u09C7\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কি এবং কেন?

Google Apps Script এ কোনো ডিরেক্টরি বিদ্যমান আছে কি না তা যাচাই করা মানে Google Drive এর মধ্যে একটি ফোল্ডারের অস্তিত্ব যাচাই করা। প্রোগ্রামাররা সাধারণত ফাইল এবং ডিরেক্টরি প্রোগ্রাম্যান্তরিকভাবে পরিচালনা করার সময় ভুল অথবা অপ্রয়োজনীয় ফোল্ডার তৈরি এড়ানোর জন্য এই যাচাইটি করে থাকেন।

## কিভাবে:

Google Apps Script ফোল্ডারের জন্য সরাসরি "exists" পদ্ধতি প্রদান করে না। পরিবর্তে, আমরা Google Drive এর অনুসন্ধান ক্ষমতাকে ব্যবহার করে যাচাই করি যে নির্দিষ্ট নামের একটি ফোল্ডার বিদ্যমান আছে কি না। এখানে একটি ধাপে ধাপে উদাহরণ দেওয়া হলো:

```javascript
// ডিরেক্টরি বিদ্যমান আছে কিনা যাচাই করার ফাংশন
function checkIfDirectoryExists(directoryName) {
  // নির্দিষ্ট নামের সাথে মিল রেখে ফোল্ডারগুলির সংগ্রহ সংগ্রহ করা
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // যাচাই করা যে নির্দিষ্ট নামের অন্তত একটি ফোল্ডার বিদ্যমান আছে কিনা
  if (folders.hasNext()) {
    Logger.log('Directory exists.');
    return true;
  } else {
    Logger.log('Directory does not exist.');
    return false;
  }
}

// উদাহরণ ব্যবহার
var directoryName = 'My Sample Folder';
checkIfDirectoryExists(directoryName);
```

নমুনা আউটপুট:
```
Directory exists.
```
অথবা
```
Directory does not exist.
```

এই স্ক্রিপ্টটি `getFoldersByName` পদ্ধতিকে কাজে লাগায় যেটা ব্যবহারকারীর Drive এর মধ্যে নির্দিষ্ট নামের মিল রেখে সব ফোল্ডারগুলি পুনরুদ্ধার করে। যেহেতু Drive এ নাম অনন্য নয়, এই পদ্ধতিটি `FolderIterator` ফেরত দেয়। এই ইটারেটরে একটি পরবর্তী আইটেম (`hasNext()`) থাকার মানে হলো ডিরেক্টরি বিদ্যমান আছে।

## গভীর ডুব

ঐতিহাসিকভাবে, ওয়েব এবং ক্লাউড পরিবেশে ফাইল ব্যবস্থাপনা বেশ উল্লেখযোগ্যভাবে উন্নতি লাভ করেছে। Google Apps Script, Google Drive এর জন্য বিস্তারিত API প্রদান করে, যা জটিল ফাইল এবং ফোল্ডার ব্যবস্থাপনা অপারেশন, সন্ধান এবং যাচাই পদ্ধতি সহ অন্যান্য চিত্তাকর্ষক কার্যাদি সম্পাদনের সুযোগ দেয়। তবে, একটি সরাসরি অস্তিত্ব যাচাই এর অভাবকে লক্ষণীয় করা যায়, যা সম্ভবত Google Drive এর একই নামের একাধিক ফোল্ডার রাখার অনুমতির কারণে, যা অনেক ফাইল সিস্টেমের সাথে বিপরীত যা একই ডিরেক্টরিতে অনন্য নামের চাহিদা করে।

এই প্রেক্ষাপটে, `getFoldersByName` পদ্ধতিটি ব্যবহার করা একটি কার্যকর প্রতিকার তবে যখন একই নামের বিশাল সংখ্যক ফোল্ডার থাকে তখন দক্ষতার সম্ভাব্য সীমাবদ্ধতা তৈরি করতে পারে। একটি বিকল্প পদ্ধতি হল দ্রুত যাচাই নিশ্চিত করতে অ্যাপ্লিকেশন-নির্দিষ্ট সূচীকরণ বা নামকরণ ধারণা বজায় রাখা, বিশেষ করে যখন কর্মক্ষমতা একটি অগ্রাধিকার হয়ে উঠে।

Google Apps Script এর পদ্ধতিটি একটি একক ফাইল সিস্টেমের সরাসরি ইন্টারফেসের সাথে কোডিং ভাষার ফাইল অস্তিত্ব যাচাইকরণের তুলনায় প্রথমে কম সরাসরি মনে হতে পারে, তবে এটি মেঘ-ভিত্তিক ফাইল স্টোরেজের জটিলতাগুলি সামাল দেওয়ার প্রয়োজনীয়তাকে প্রতিফলিত করে। Google Apps Script ব্যবহার করে Drive ব্যবস্থাপনা করা ডেভেলপারদের উচিত Google Drive এর শক্তিমত্তা এবং সীমাবদ্ধতা অনুকূলে অপ্টিমাইজ করা, এই বিষয়গুলি বিবেচনায় রাখা।
