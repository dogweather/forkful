---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:06.924537-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u098F, `UrlFetchApp`\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8 \u0993\u09AF\u09BC\u09C7\u09AC \u0995\
  \u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AE\u09C2\u09B2 \u09AD\u09BF\u09A4\u09CD\
  \u09A4\u09BF\u0964 \u09A8\u09BF\u099A\u09C7 \u098F\u0995\u099F\u09BF \u09A7\u09BE\
  \u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\
  \u09BF\u0995\u09BE \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.522711-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F, `UrlFetchApp` \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\
  \u09B8 \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\
  \u099F \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AE\u09C2\u09B2 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0964 \u09A8\u09BF\
  \u099A\u09C7 \u098F\u0995\u099F\u09BF \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09BF\u0995\u09BE \u098F\u09AC\
  \u0982 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2, \u09AF\
  \u09BE \u09A6\u09C7\u0996\u09BE\u09AF\u09BC \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\u09C7\
  \u09B0 \u098F\u0987\u099A\u099F\u09BF\u098F\u09AE\u098F\u09B2 \u0995\u09A8\u09CD\
  \u099F\u09C7\u09A8\u09CD\u099F \u09AB\u09C7\u099A \u0995\u09B0\u09C7 \u098F\u09AC\
  \u0982 \u09B2\u0997 \u0995\u09B0\u09C7."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
Google Apps Script এ, `UrlFetchApp` সার্ভিস ওয়েব কন্টেন্ট ডাউনলোডের জন্য মূল ভিত্তি। নিচে একটি ধাপে ধাপে নির্দেশিকা এবং একটি সাধারণ উদাহরণ দেওয়া হল, যা দেখায় কিভাবে একটি ওয়েবপেজের এইচটিএমএল কন্টেন্ট ফেচ করে এবং লগ করে:

1. **বেসিক ফেচ অপারেশন:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- এই কোডটি example.com এর এইচটিএমএল কন্টেন্ট ফেচ করে এবং তা লগ করে। কোনো অতিরিক্ত প্যারামিটার ছাড়া একটি ওয়েব পেজের সোর্স পেতে এটি একটি সোজাসাপটা উদাহরণ।

2. **রিডিরেক্ট এবং HTTPS সম্পর্কিত বিষয় নির্বাহ:**

HTTPS অথবা রিডিরেক্ট হ্যান্ডেলিং এর জন্য, কোড প্রায় একই রকম থাকে, তবে রিডিরেক্টের জন্য নির্দিষ্ট অপশন বা এরর হ্যান্ডেলিং বাস্তবায়ন করা বিবেচ্য:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // স্বয়ংক্রিয়ভাবে রিডিরেক্টস অনুসরণ করে
    'muteHttpExceptions': true // সম্ভাব্য এক্সেপশনগুলিকে মিউট করে সহনশীলভাবে নির্বাহ করে
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **রেট সীমা ও কোটা:**

Google Apps Script এর কোটা সম্পর্কে সচেতন থাকুন; ভারী ব্যবহার রেট সীমা জন্য এরর হ্যান্ডেলিং প্রয়োজন হতে পারে।

## গভীর ডুব
ইতিহাসে, ওয়েব কন্টেন্ট ডাউনলোড এবং ম্যানিপুলেশন সাধারণ এইচটিপি অনুরোধ দিয়ে শুরু হয়েছিল, এবং স্ক্রিপ্টিং ভাষার আবির্ভাব দিয়ে ব্যাপকভাবে উন্নত হয়েছে। Google Apps Script জি স্যুট ইকোসিস্টেমের মধ্যে এইরূপ টাস্ক সরলভাবে বাস্তবায়ন করে, Google এর শক্তিশালী অবকাঠামো কাজে লাগিয়ে। `UrlFetchApp` সার্ভিস এই কার্যকারিতার একটি মূল উপাদান, জটিল HTTP/S অনুরোধকে আরো সরল অ্যাপ্লিকেশন-স্তরের ইন্টারফেসে পরিণত করে।

সুবিধাজনক সত্ত্বেও, Google Apps Script ওয়েব স্ক্র্যাপিং এর জন্য অথবা ফেচ করা ডাটার জটিল পোস্ট-প্রসেসিং প্রয়োজন হলে সর্বদা সর্বোত্তম টুল নাও হতে পারে যেহেতু Google দ্বারা বিধিবদ্ধ এক্সিকিউশন সময় সীমা ও কোটা প্রযোজ্য। এমন ক্ষেত্রে, ডেডিকেটেড ওয়েব স্ক্র্যাপিং ফ্রেমওয়ার্ক অথবা অ্যাসিঙ্ক্রোনাস আই/ও অপারেশনের জন্য ডিজাইন করা ভাষা, যেমন Node.js সাথে Puppeteer অথবা Cheerio মত লাইব্রেরী বেশি নমনীয়তা এবং শক্তি অফার করতে পারে।

তাছাড়া, Google Apps Script Google সেবা (যেমন Sheets, Docs, এবং Drive) এর সাথে ইন্টিগ্রেট করার এবং হালকা ডাটা ফেচ অপারেশন সঞ্চালন করার জন্য দুর্দান্ত একটি টুল হলেও, এর এক্সিকিউশন পরিবেশের সীমাবদ্ধতাগুলি মনে রাখা জরুরি। চাপপূর্ণ টাস্কের জন্য, Google Cloud Functions অথবা Apps Script এর অ্যাডভান্সড সার্ভিস ব্যবহার করে প্রসেসিং এর জন্য বাহ্যিক কম্পিউট রিসোর্স বিবেচনা করুন।
