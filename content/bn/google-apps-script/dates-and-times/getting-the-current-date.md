---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:47.241175-06:00
description: "Google Apps Script \u098F \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u09B2 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0993 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE, \u09AF\u09C7\u099F\
  \u09BE \u0997\u09C1\u0997\u09B2\u09C7\u09B0 \u0987\u0995\u09CB\u09B8\u09BF\u09B8\
  \u09CD\u099F\u09C7\u09AE\u09C7 \u09AC\u09BE\u0981\u09A7\u09BE \u0985\u09CD\u09AF\
  \u09BE\u09AA\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u0995\u09BE\u099C \u0985\u099F\
  \u09CB\u09AE\u09C7\u09B6\u09A8, \u09B2\u0997\u09BF\u0982, \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.536983-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u0993 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE, \u09AF\u09C7\u099F\u09BE\
  \ \u0997\u09C1\u0997\u09B2\u09C7\u09B0 \u0987\u0995\u09CB\u09B8\u09BF\u09B8\u09CD\
  \u099F\u09C7\u09AE\u09C7 \u09AC\u09BE\u0981\u09A7\u09BE \u0985\u09CD\u09AF\u09BE\
  \u09AA\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u0995\u09BE\u099C \u0985\u099F\u09CB\
  \u09AE\u09C7\u09B6\u09A8, \u09B2\u0997\u09BF\u0982, \u098F\u09AC\u0982\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কি এবং কেন?

Google Apps Script এ বর্তমান তারিখ পাওয়া মানে হল সরাসরি তারিখ ও সময় আনা, যেটা গুগলের ইকোসিস্টেমে বাঁধা অ্যাপগুলিতে কাজ অটোমেশন, লগিং, এবং টাইমস্ট্যাম্পিং এর জন্য একটা সাধারণ কাজ। প্রোগ্রামাররা এটা গুগল ডকস, শীটস, এবং গুগলের অন্যান্য সার্ভিসগুলিতে ডাইনামিক কনটেন্ট জেনারেশন, ডেডলাইন ট্র্যাকিং, এবং শিডিউলিং এর জন্য ব্যবহার করে।

## কিভাবে:

Google Apps Script, যা JavaScript এ ভিত্তি করে তৈরি, বর্তমান তারিখ পাওয়ার জন্য সোজা পথ প্রদান করে। আপনি `new Date()` কনস্ট্রাক্টর ব্যবহার করে একটি নতুন তারিখ অবজেক্ট তৈরি করতে পারেন যা বর্তমান তারিখ ও সময়কে প্রতিনিধিত্ব করে। এখানে আপনি কিভাবে এইটাকে বিভিন্ন ফরম্যাটে ম্যানিপুলেট এবং ডিসপ্লে করতে পারেন তা উল্লেখ করা হল।

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // স্ক্রিপ্টের টাইমজোনে বর্তমান তারিখ ও সময় লগ করে
  
  // শুধুমাত্র তারিখটি YYYY-MM-DD ফরম্যাটে ডিসপ্লে করতে
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // উদাহরণ আউটপুট: "2023-04-01"
  
  // আরও পঠনযোগ্য ফরম্যাটে ডিসপ্লে করা
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // উদাহরণ আউটপুট: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

এই কোড স্নিপেটগুলি দেখায় কিভাবে বর্তমান তারিখ ও সময় ধরা এবং ফরম্যাট করা যায়, Google Apps Script এর বিভিন্ন প্রোগ্রামিং চাহিদায় ভিন্নতা প্রদর্শন করে।

## গভীর ডাইভ

`Date` অবজেক্টের উপর JavaScript নির্ধারিত হওয়ার আগে, প্রোগ্রামারদেরকে সময় এবং তারিখের হিসেব নিজেদের হাতে রাখতে হত, যেটা বিশেষভাবে মানকরূপ না হওয়া এবং বেশি জটিল উপায়ে করতে হত। এটা টাইমস্ট্যাম্প ইন্টিজার এবং হোমমেড তারিখ ফাংশনের ব্যবহার অন্তর্ভুক্ত করত, যা এক প্রোগ্রামিং পরিবেশ থেকে আরেক পরিবেশে ভিন্ন হত, যার ফলে অসঙ্গতি এবং সামঞ্জস্যতা সমস্যা সৃষ্টি হত।

JavaScript এ, এবং এর প্রসারিত রূপে Google Apps Script এ `new Date()` অবজেক্টের চালু করা, তারিখ এবং সময় অপারেশনগুলিকে আরও সহজবোধ্য করেছে এবং তারিখ-সংশ্লিষ্ট অপারেশনে প্রয়োজনীয় কোডের পরিমাণ কমিয়েছে। উল্লেখ্য, যদিও Google Apps Script এর বাস্তবায়ন বহু অ্যাপ্লিকেশনে গুগলের পণ্য সমূহের জন্য সুবিধাজনক এবং যথেষ্ট, তবুও এটি সম্ভবত সব সিনারিও সামলাতে পারে না, বিশেষ করে যেসব কঠিন টাইম-জোন হ্যান্ডলিং অথবা দ্রুতগতির পরিবেশে নির্ভুল টাইম-স্ট্যাম্প লগিং প্রয়োজনীয়।

এরকম উন্নত ব্যবহারের জন্য, প্রোগ্রামাররা প্রায়ই JavaScript এ Moment.js অথবা date-fns এর মত লাইব্রেরিগুলির দিকে ঝুঁকে, যদিও Google Apps Script এই লাইব্রেরিগুলি স্বতন্ত্রভাবে সমর্থন করে না, ডেভেলপাররা পাওয়া JavaScript তারিখ মেথড ব্যবহার করে অথবা HTML সার্ভিস অথবা Apps Script এর URL Fetch সার্ভিসের মাধ্যমে বাইরের লাইব্রেরি পৌঁছানোর মাধ্যমে কিছু ফাংশনালিটির অনুকরণ করতে পারে। যাইহোক, Google Apps Script এর স্বদেশী তারিখ এবং সময় ফাংশনের সরলতা এবং সংহতি গুগল ইকোসিস্টেমের বেশিরভাগ কাজের জন্য প্রাথমিক অগ্রাধিকার থাকে।
