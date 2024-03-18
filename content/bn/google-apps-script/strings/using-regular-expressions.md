---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:15.443874-06:00
description: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8\u09B8 (regex) \u09B9\u09B2 \u09AA\u09CD\u09AF\u09BE\
  \u099F\u09BE\u09B0\u09CD\u09A8 \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u099A\u09B0\u09BF\u09A4\u09CD\u09B0\u09C7\
  \u09B0 \u09B8\u09AE\u09A8\u09CD\u09AC\u09AF\u09BC\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09C7\u09B2\u09BE\u09A4\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\
  \u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0996\u09CB\u0981\u099C\u09BE, \u09B8\
  \u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09BE \u09AC\u09BE \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.512769-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8\u09B8 (regex) \u09B9\u09B2 \u09AA\u09CD\u09AF\u09BE\
  \u099F\u09BE\u09B0\u09CD\u09A8 \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u099A\u09B0\u09BF\u09A4\u09CD\u09B0\u09C7\
  \u09B0 \u09B8\u09AE\u09A8\u09CD\u09AC\u09AF\u09BC\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09C7\u09B2\u09BE\u09A4\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\
  \u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0996\u09CB\u0981\u099C\u09BE, \u09B8\
  \u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09BE \u09AC\u09BE \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

রেগুলার এক্সপ্রেশনস (regex) হল প্যাটার্ন যা স্ট্রিংগুলিতে চরিত্রের সমন্বয়গুলি মেলাতে ব্যবহৃত হয়। প্রোগ্রামাররা তাদের খোঁজা, সম্পাদনা বা টেক্সট এবং ডেটা নিয়ন্ত্রণের জন্য ব্যবহার করে থাকেন, যা প্যাটার্ন মেলানো এবং ডেটা পার্সিং কাজগুলিতে অপরিহার্য।

## কিভাবে:

Google Apps Script-এ রেগুলার এক্সপ্রেশনস ব্যবহার করা জাভাস্ক্রিপ্ট-ভিত্তিক সিনট্যাক্সের কারণে সহজ। এখানে আপনি খোঁজা এবং ডেটা যাচাইকরণের মতো সাধারণ কার্যগুলির জন্য আপনার স্ক্রিপ্টে রেগুলার এক্সপ্রেশনস যোগ করার জন্য যেমন করতে পারেন:

### স্ট্রিংগুলি খোঁজা

ধরুন, আপনি এমন একটি স্ট্রিং খুঁজতে চান যা নির্দিষ্ট একটি প্যাটার্ন, যেমন একটি ইমেইল ঠিকানা ধারণ করে। এখানে একটি সাধারণ উদাহরণ:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("পাওয়া গেছে: " + found[0]);
  } else {
    Logger.log("কোনো ইমেইল পাওয়া যায়নি।");
  }
}

// নমুনা ব্যবহার
findEmailInText("আমাদের যোগাযোগ করুন info@example.com-এ।");
```

### ডেটা যাচাইকরণ

ডেটা যাচাইকরণে রেগুলার এক্সপ্রেশনস উজ্জ্বল ভাবে কাজ করে। নিচের একটি ফাংশন এমন একটি ইনপুট স্ট্রিং যাচাই করে যা একটি সাধারণ পাসওয়ার্ড নীতি (অন্তত একটি বড়হাতের অক্ষর, একটি ছোটহাতের অক্ষর, এবং সর্বনিম্ন ৮ অক্ষর) অনুসরণ করে কিনা তা যাচাই করে।

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// নমুনা আউটপুট
Logger.log(validatePassword("Str0ngPass")); // আউটপুট: true
Logger.log(validatePassword("weak"));       // আউটপুট: false
```

## গভীরে ডুব দেওয়া

Google Apps Script-এ রেগুলার এক্সপ্রেশনস জাভাস্ক্রিপ্ট থেকে উত্তরাধিকার সূত্রে পাওয়া যায়, যা প্রথম ECMAস্ক্রিপ্ট ভাষা স্পেসিফিকেশনে জুন 1997-এ মানকীকৃত হয়েছিল। যদিও শক্তিশালী, অত্যধিক ব্যবহার বা জটিল প্যাটার্ন ম্যাচিং কাজের জন্য ব্যবহার করায় কখনও কখনও তা বিভ্রান্তিকর এবং রক্ষণাবেক্ষণ করা কঠিন কোড হয়ে দাঁড়াতে পারে।

উদাহরণস্বরূপ, HTML বা XML পার্সিংয়ের জন্য আপনি যদি রেগুলার এক্সপ্রেশনস ব্যবহার করেন, তবে সাধারণত তা উৎসাহিত হয় না কারণ এই নথিগুলির জটিল এবং গুণ্ঠিত কাঠামো। এর পরিবর্তে, HTML-এর জন্য DOM পার্সারের মতো বিশেষভাবে ডিজাইনকৃত টুলগুলি বেশি বিশ্বস্ত এবং পঠনযোগ্য।

তদুপরি, Google Apps Script ডেভেলপারদের বড় পরিসরের টেক্সট পরিচালনা কাজে জটিল রেগুলার এক্সপ্রেশনের প্যাটার্ন ব্যবহার করার সময় সম্ভাব্য পারফরম্যান্স ইস্যুগুলি সম্পর্কে সচেতন থাকা উচিত, কারণ রেগুলার এক্সপ্রেশন প্রসেসিং সিপিইউ-ইন্টেনসিভ হতে পারে। এমন ক্ষেত্রে, কাজটিকে সরল উপ-কাজগুলিতে ভাগ করা বা অন্তর্নির্মিত স্ট্রিং পরিচালনা ফাংশনগুলি ব্যবহার করা পারফরম্যান্স এবং রক্ষণাবেক্ষণের একটি ভালো ভারসাম্য প্রদান করতে পারে।