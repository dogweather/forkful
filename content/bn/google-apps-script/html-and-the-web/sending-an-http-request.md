---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:19.673774-06:00
description: "Google Apps Script \u098F HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0993\u09AF\u09BC\u09C7\u09AC\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE API \u098F\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09A4\u09CD\u09AE\u0995\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0995\u09B2 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\
  \u09AC\u09BE\u0997\u09C1\u09B2\u09BF \u09A5\u09C7\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.520583-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\
  \u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE API \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09A4\
  \u09CD\u09AE\u0995\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09B2\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\u09BE\
  \u0997\u09C1\u09B2\u09BF \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE\
  \ \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7\u0964 \u0989\u09A8\u09CD\u09A8\
  \u09A4 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09AE\u09CD\u09AA\u09A6 \u098F\u09AC\
  \u0982 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09BE\u09B0\u09BF\u09A4\u09BE \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF \u09A4\u09BE\u09A6\u09C7\u09B0 Google Apps Script\
  \ \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u09C7 \u09B8\u09AE\u09A8\u09CD\u09AC\
  \u09BF\u09A4 \u0995\u09B0\u09C7\u0964."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
Google Apps Script এ, HTTP অনুরোধ পাঠানোর প্রাথমিক উপায় হলো `UrlFetchApp` সেবাটি ব্যবহার করা। এই সেবাটি HTTP GET এবং POST অনুরোধ করার পদ্ধতি সরবরাহ করে। নিম্নলিখিত একটি সাধারণ উদাহরণ যেখানে JSON ডেটা পুনরুদ্ধারের জন্য GET অনুরোধ করা হয়েছে:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

একটি POST অনুরোধের জন্য, যা সাধারণত সার্ভারে ডেটা পাঠানোর জন্য ব্যবহৃত হয়, আপনাকে বিকল্প প্যারামিটারে আরও বিস্তারিত অন্তর্ভুক্ত করতে হবে:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // জাভাস্ক্রিপ্ট অবজেক্টটি JSON স্ট্রিংয়ে রূপান্তরিত করুন
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

এই স্নিপেটগুলি বেসিক GET এবং POST অনুরোধের বাস্তবায়ন দেখায়। আউটপুট আসবে API প্রতিউত্তর অনুযায়ী এবং Google Apps Script's Logger এ দেখা যাবে।

## গভীরে:
Google Apps Script’s `UrlFetchApp` সেবা এর প্রচলন থেকে অনেক উন্নত হয়েছে, হেডার সেট করা, পেলোড, এবং ফাইল আপলোডের জন্য মাল্টিপার্ট/ফর্ম-ডেটা হ্যান্ডলিং এর মত বৈশিষ্ট্য সহ HTTP অনুরোধে আরও সুক্ষ্ণ নিয়ন্ত্রণ সরবরাহ করে। এটি বাহ্যিক ওয়েব সেবাগুলি একীভূত করার একটি সরাসরি উপায় সরবরাহ করে থাকলেও, আরও শক্তিশালী ব্যাকএন্ড ভাষায় কর্মরত ডেভেলপারদের কাছে এর কার্যকারিতা পাইথনের `requests` বা Node.js -এর JavaScript `fetch` API এর চেয়ে কিছুটা সীমাবদ্ধ মনে হতে পারে।

একটি উল্লেখযোগ্য সীমাবদ্ধতা হলো Google Apps Script এর জন্য নির্ধারিত এক্সিকিউশন সময় সীমা, যা দীর্ঘমেয়াদী অনুরোধগুলিকে প্রভাবিত করে। অধিকন্তু, `UrlFetchApp` বিস্তৃত ব্যবহারের ক্ষেত্র কভার করলেও, OAuth প্রমাণীকরণ অথবা অত্যন্ত বড় পেলোড হ্যান্ডলিং জড়িত জটিল পরিস্থিতিগুলি সৃজনশীল সমাধান অথবা অতিরিক্ত Google Cloud সম্পদ ব্যবহারের প্রয়োজন হতে পারে।

তবুও, Google Workspace ডেভেলপারদের সম্মুখীন হয়ে থাকা অধিকাংশ সংযোগের জন্য - ডেটা উত্তোলন থেকে বাহ্যিক সেবাগুলিতে আপডেট প্রেরণের মতো কাজগুলিতে - `UrlFetchApp` একটি শক্তিশালী, প্রবেশযোগ্য সরঞ্জাম প্রদান করে। এর Google Apps Script এ একীভূতকরণ মানে বাহ্যিক লাইব্রেরি অথবা জটিল সেটআপের প্রয়োজন নেই, যা Google Apps Script এর সীমানাগুলির মধ্যে HTTP অনুরোধগুলি তুলনামূলকভাবে সহজভাবে কার্যকর করাকে সহজ করে তোলে। ওয়েব APIs এর জগতের প্রসার অব্যাহত থাকায়, `UrlFetchApp` Google Apps Script প্রোগ্রামগুলির জন্য Google এর ইকোসিস্টেমের বাইরের পৃথিবীর সাথে মিথস্ক্রিয়া করার একটি ক্রিটিক্যাল ব্রিজ হিসেবে থাকে।
