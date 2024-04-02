---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:46.208719-06:00
description: "JSON, \u09AC\u09BE JavaScript Object Notation, \u09B9\u09B2 \u09A1\u09C7\
  \u099F\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u098F\u09AC\u0982 \u09AA\
  \u09B0\u09BF\u09AC\u09B9\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\
  \u09B8, \u09AF\u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0-\u099F\u09C1-\u0995\
  \u09CD\u09B2\u09BE\u09DF\u09C7\u09A8\u09CD\u099F \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\
  \u0997 \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\
  \u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:43.549293-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u09AC\u09BE JavaScript Object Notation, \u09B9\u09B2 \u09A1\u09C7\
  \u099F\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u098F\u09AC\u0982 \u09AA\
  \u09B0\u09BF\u09AC\u09B9\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\
  \u09B8, \u09AF\u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0-\u099F\u09C1-\u0995\
  \u09CD\u09B2\u09BE\u09DF\u09C7\u09A8\u09CD\u099F \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\
  \u0997 \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\
  \u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?

JSON, বা JavaScript Object Notation, হল ডেটা সংরক্ষণ এবং পরিবহনের জন্য একটি হালকা বিন্যাস, যা সার্ভার-টু-ক্লায়েন্ট যোগাযোগ এবং কনফিগারেশন ফাইলের জন্য আদর্শ। প্রোগ্রামাররা Google Apps Script-এ Google সেবাগুলো (যেমন Sheets, Docs, Drive) এবং বাহ্যিক সোর্সের মাঝে সহজে ডেটা বিনিময়ের জন্য এটি ব্যবহার করে, এর মানুষের পড়তে সক্ষম গঠন এবং JavaScript-ভিত্তিক পরিবেশে সহজে মিলে যাওয়ার কারণে।

## কিভাবে:

Google Apps Script-এ, JSON নিয়ে কাজ করা একটি সহজ প্রক্রিয়া, মূলত JavaScript দ্বারা JSON পার্সিং এবং স্ট্রিংগিফাইয়েশনের জন্য দেশীয় সাপোর্টের কারণে। এখানে কিছু সাধারণ অপারেশন আছে:

**1. JSON পার্স করা**: ধরুন আমরা একটি ওয়েব সার্ভিস থেকে একটি JSON স্ট্রিং পেয়েছি; এটি একটি JavaScript অবজেক্টে পার্স করা ডেটা ম্যানিপুলেশনের জন্য অপরিহার্য।

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // আউটপুট: Sample Project
```

**2. JavaScript অবজেক্টস কে JSON স্ট্রিং এ রূপান্তর**: অন্যদিকে, একটি JavaScript অবজেক্টকে একটি JSON স্ট্রিং এ রূপান্তর করা তখন উপকারী হয়, যখন আমাদের ডেটা Apps Script থেকে একটি বহিরাগত পরিষেবায় পাঠানো দরকার।

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // আউটপুট: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. জটিল ডেটার সাথে কাজ করা**:
অবজেক্টের অ্যারেসমূহের মতো আরো জটিল ডেটা গঠনের ক্ষেত্রে, প্রক্রিয়া একই থাকে, JSON-এর ডেটা প্রতিনিধিত্বের জন্য এর নমনীয়তা প্রদর্শন করা।

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // আউটপুট: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## গভীর ডুব

JSON-এর আধুনিক ওয়েব অ্যাপ্লিকেশনগুলিতে ব্যাপক ব্যবহার অত্যন্ত গুরুত্বপূর্ণ, যা এর সহজ গঠন এবং JavaScript-এর সাথে সহজে মিলে যাওয়ার ক্ষমতায় মূলত নির্ভরশীল, যা ওয়েবের ভাষা। এর নকশা, JavaScript অবজেক্ট লিটারাল দ্বারা অনুপ্রাণিত, যদিও কঠোর, এর দ্রুত গ্রহণের পথ সুগম করে। ২০০০-এর দশকে, AJAX-চালিত ওয়েব অ্যাপ্লিকেশনগুলির জন্য XML-এর একটি বিকল্প হিসেবে JSON জনপ্রিয়তা লাভ করে, যা একটি হালকা এবং কম বেশি বিস্তৃত ডেটা আদানপ্রদানের ফর্ম্যাট প্রস্তাব করে। Google Apps Script-এর বিভিন্ন Google API এবং বহিরাগত পরিষেবাগুলির সাথে গভীর একীকরণের কারণে, JSON এই প্ল্যাটফর্মগুলিতে ডেটা গঠন, পরিবহন, এবং ম্যানিপুলেশনের জন্য একটি কেন্দ্রীয় ফর্ম্যাট হিসেবে কাজ করে।

ওয়েব অ্যাপ্লিকেশনগুলির জন্য JSON যখন প্রধানত ব্যবহার করা হয়, তখন কনফিগারেশন ফাইলের জন্য YAML বা উচ্চ-কর্মক্ষম পরিবেশে আরও দক্ষ বাইনারি সিরিয়ালাইজেশনের জন্য Protobuf এর মতো বৈকল্পিক ডেটা ফর্ম্যাট বিদ্যমান। তবে, JSON-এর পঠনীয়তা, ব্যবহারের সহজতা, এবং বিভিন্ন প্রোগ্রামিং ভাষা এবং টুলগুলিতে ব্যাপক সমর্থনের ভারসাম্য এটিকে Google Apps Script এবং তার বাইরে অনেধিক ডেভেলপারের জন্য ডিফল্ট পছন্দ হিসাবে স্থির করে তোলে।
