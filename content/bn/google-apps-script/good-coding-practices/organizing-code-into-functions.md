---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:55:44.784970-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script-\u098F, \u09AF\
  \u09C7\u099F\u09BF JavaScript \u098F\u09B0 \u0989\u09AA\u09B0 \u09AD\u09BF\u09A4\
  \u09CD\u09A4\u09BF \u0995\u09B0\u09C7, \u0986\u09AA\u09A8\u09BF `function` \u0995\
  \u09C0\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u0982\u099C\u09CD\
  \u099E\u09BE\u09AF\u09BC\u09BF\u09A4 \u0995\u09B0\u09C7\u09A8, \u098F\u09B0 \u09AA\
  \u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09A8\u09A8\u09CD\u09AF \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09A8\u09BE\u09AE,\u2026"
lastmod: '2024-03-17T18:47:43.530779-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script-\u098F, \u09AF\u09C7\u099F\u09BF JavaScript \u098F\u09B0\
  \ \u0989\u09AA\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF \u0995\u09B0\u09C7, \u0986\
  \u09AA\u09A8\u09BF `function` \u0995\u09C0\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09AF\u09BC\u09BF\u09A4 \u0995\
  \u09B0\u09C7\u09A8, \u098F\u09B0 \u09AA\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0985\
  \u09A8\u09A8\u09CD\u09AF \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09BE\u09AE, \u09AC\
  \u09A8\u09CD\u09A7\u09A8\u09C0 `()` \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09AA\u09CD\
  \u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\u099F\u09BE\u09B0 \u09A5\u09BE\u0995\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7, \u098F\u09AC\u0982 \u0995\u09BE\u09B0\u09CD\u09B2\u09BF\
  \ \u09AC\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09C7\u099F `{}` \u09AF\u09BE \u09AB\
  \u09BE\u0982\u09B6\u09A8\u09C7\u09B0 \u0995\u09CB\u09A1 \u09AC\u09CD\u09B2\u0995\
  \u0995\u09C7 \u0986\u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  ."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB"
weight: 18
---

## কিভাবে:
Google Apps Script-এ, যেটি JavaScript এর উপর ভিত্তি করে, আপনি `function` কীওয়ার্ড ব্যবহার করে ফাংশন সংজ্ঞায়িত করেন, এর পরে একটি অনন্য ফাংশন নাম, বন্ধনী `()` যেখানে প্যারামিটার থাকতে পারে, এবং কার্লি ব্র্যাকেট `{}` যা ফাংশনের কোড ব্লককে আবদ্ধ করে। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Hello, ' + user + '!');
}

greetUser();
```

নমুনা আউটপুট:

```
Hello, someone@example.com!
```

এখন, চলুন Google Sheets সম্পর্কিত একটি বাস্তব উদাহরণ বিবেচনা করি যেখানে আমরা কার্যকারিতা দুইটি ফাংশনে বিভক্ত করেছি: একটি শীট সেটআপের জন্য এবং অন্যটি এটিকে ডাটা দিয়ে পূরণের জন্য।

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Sales Data');
  sheet.appendRow(['Item', 'Quantity', 'Price']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Sales Data');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// ডাটার অ্যারে ইনিশিয়ালাইজ করুন
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// ফাংশনগুলো চালান
setupSheet();
populateSheet(salesData);
```

এই উদাহরণে, `setupSheet` শীটটিকে প্রস্তুত করে, এবং `populateSheet` বিক্রয় ডাটার একটি অ্যারে নিয়ে শীটটিকে পূরণ করে। এই উদ্বেগগুলোকে পৃথক করে দেওয়া কোডকে আরও পরিষ্কার এবং পরিবর্তনের জন্য অধিক অভিযোজ্য করে তোলে।

## গভীর ডুব
কোডকে ফাংশনে বিভক্ত করার ধারণাটি নতুন নয় বা Google Apps Script-এ অনন্য নয়; এটি প্রায় সমস্ত প্রোগ্রামিং ভাষায় সমর্থিত একটি মৌলিক প্রোগ্রামিং অনুশীলন। ঐতিহাসিকভাবে, ফাংশনগুলি ইনপুটকে আউটপুটে ম্যাপিং করার গাণিতিক ধারণা থেকে বিকশিত হয়েছে, যা গঠনমূলক প্রোগ্রামিংয়ে একটি মূল স্তম্ভ হয়ে উঠেছে। এই পদ্ধতিটি মডিউলারতা এবং কোড পুনঃব্যবহারকে উৎসাহিত করে, স্ক্রিপ্টের বিভিন্ন অংশগুলির পরীক্ষা করার জন্য স্পষ্ট পথ প্রদান করে।

JavaScript-ভিত্তিক হওয়ায়, Google Apps Script অত্যন্ত উপকৃত হয় JavaScript-এর প্রথম শ্রেণীর ফাংশনগুলি থেকে, যা অন্য ফাংশনগুলির থেকে আর্গুমেন্ট হিসেবে পাস করা, ফেরত দেওয়া এবং ভেরিয়েবলে নিয়োগ করা যেতে পারে। এই বৈশিষ্ট্যটি কলব্যাক এবং কার্যকরী প্রোগ্রামিং এর মতো উন্নত প্যাটার্ন উন্মুক্ত করে, যদিও এই প্যাটার্নগুলি Google Apps Script-এ সাধারণ অটোমেশন টাস্কের জন্য অনাবশ্যক জটিলতা সৃষ্টি করতে পারে।

বৃহত্তর প্রকল্প বা জটিলতর অ্যাপ্লিকেশন নিয়ে কাজ করার জন্য, ডেভেলপাররা JavaScript-এর নতুন বৈশিষ্ট্য যেমন অ্যারো ফাংশনগুলি, অ্যাসিঙ্ক/await অ্যাসিঙ্ক্রোনাস অপারেশনের জন্য, এবং এমনকি TypeScript স্ট্যাটিক টাইপিং-এর জন্য ব্যবহার করতে পারেন। TypeScript, বিশেষত, Google Apps Script হিসেবে কম্পাইল করা যায়, যা ডেভেলপারদের জন্য আরো কঠিন টাইপ চেকিং এবং উন্নত অবজেক্ট-অরিয়েন্টেড বৈশিষ্ট্যগুলি সন্ধান করার একটি পথ প্রদান করে।

তবে, Google Apps স্যুটের মধ্যে অধিকাংশ স্ক্রিপ্টিংয়ের প্রয়োজনের জন্য, প্রদর্শিত সাধারণ, ভালোভাবে সাজানো ফাংশনগুলির মতো সহজ মাধ্যমে চলা একটি দৃঢ় ভিত্তি সরবরাহ করে। দক্ষতার জন্য উন্নত বৈশিষ্ট্য ব্যবহার এবং রক্ষণাবেক্ষণ এবং পাঠযোগ্যতার সৌজন্যে সহজতা বজায় রাখার মধ্যে সবসময় একটি সামঞ্জস্যপূর্ণ অবস্থান থাকে।
