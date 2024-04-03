---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:55.630438-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u098F, \u098F\
  \u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\
  \u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\
  \ DriveApp \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE Google Drive \u098F \u09AB\u09BE\u0987\
  \u09B2 \u09A4\u09C8\u09B0\u09BF, \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09AE\
  \u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\
  \u09B0\u09B2\u2026"
lastmod: '2024-03-17T18:47:43.547053-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F, \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\
  \u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE \u09AF\u09BE\u09AF\u09BC DriveApp \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\
  \u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE\
  \ Google Drive \u098F \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF, \u09AA\u09A1\
  \u09BC\u09BE \u098F\u09AC\u0982 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\
  \u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF\
  \ \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u09A4\u09BE\u09A4\u09C7\
  \ \u0995\u09BF\u099B\u09C1 \u09A1\u09C7\u099F\u09BE \u09B2\u09BF\u0996\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8, \u098F\u09AC\u0982 \u09AA\u09B0\u09C7 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u09B6\u09C7\u09B7\u09C7 \u09A4\u09BE \u09B8\u09B0\
  \u09BF\u09DF\u09C7 \u09AB\u09C7\u09B2\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  ."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
Google Apps Script এ, একটি অস্থায়ী ফাইল তৈরি করা যায় DriveApp সার্ভিস ব্যবহার করে, যা Google Drive এ ফাইল তৈরি, পড়া এবং মুছে ফেলার একটি সরল পদ্ধতি প্রদান করে। এখানে কিভাবে আপনি একটি অস্থায়ী টেক্সট ফাইল তৈরি করতে পারেন, তাতে কিছু ডেটা লিখতে পারেন, এবং পরে ব্যবহার শেষে তা সরিয়ে ফেলতে পারেন:

```javascript
function createTemporaryFile() {
  // "tempFile.txt" নামে একটি অস্থায়ী ফাইল তৈরি করা
  var tempFile = DriveApp.createFile('tempFile.txt', 'Temporary content', MimeType.PLAIN_TEXT);
  
  // অ্যাক্সেস বা ডিবাগিং জন্য ফাইল URL লগ করা
  Logger.log('Temporary file created: ' + tempFile.getUrl());
  
  // উদাহরণ অপারেশন: ফাইল কন্টেন্ট পড়া
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Content of tempFile: ' + content);
  
  // অপারেশন সম্পূর্ণ এবং আর ফাইলের দরকার নেই বলে আসুমি করা
  // অস্থায়ী ফাইল সরানো
  tempFile.setTrashed(true);
  
  // মুছে ফেলা নিশ্চিত করা
  Logger.log('Temporary file deleted');
}
```

এই স্ক্রিপ্ট চালালে আউটপুট হবে:

```
Temporary file created: [তৈরি করা অস্থায়ী ফাইলের URL]
Content of tempFile: Temporary content
Temporary file deleted
```

এই উদাহরণ স্ক্রিপ্ট একটি অস্থায়ী ফাইল তৈরি করা, এর কন্টেন্ট পড়ে দেখা, এবং অবশেষে, ফাইল সরিয়ে পরিষ্কার করার প্রক্রিয়া দেখায়।

## গভীর ডাইভ
সফটওয়্যার ডেভেলপমেন্টে অস্থায়ী ফাইল তৈরির ধারণা ফাইল ম্যানেজমেন্টের ধারণা প্রাচীন যেমন। ঐতিহ্যগত ফাইল সিস্টেমগুলিতে, অস্থায়ী ফাইলগুলি প্রায়ই নির্দিষ্ট টেম্প ডিরেক্টরিগুলিতে তৈরি করা হয় এবং বড় ডেটাসেটগুলি সর্টিং, ওয়েব অ্যাপ্লিকেশনগুলির জন্য সেশন ডেটা ধারণ করা, বা ফাইল রূপান্তর প্রক্রিয়াগুলিতে ডেটার চাঙ্কগুলি স্টোর করা জন্য বিভিন্ন মধ্যবর্তী প্রক্রিয়াগুলির জন্য অপরিহার্য।

Google Apps Script এ, অস্থায়ী ফাইল তৈরির প্রক্রিয়া Google Drive এর অবকাঠামোকে লিভারেজ করে, যা মেঘ-ভিত্তিক ফাইল ম্যানেজমেন্ট এবং ঐতিহ্যগত প্রোগ্রামিং ধারণাগুলির এক আকর্ষণীয় মিশ্রণ অফার করে। তবে, Google Drive এ অস্থায়ী ফাইল তৈরির এই পদ্ধতির তার সীমাবদ্ধতা এবং ব্যয় রয়েছে, Google Drive যে কোটা সীমা আরোপ করে তা বিবেচনা করে। তাছাড়া, একটি স্থানীয় ফাইলসিস্টেমের তুলনায় Google Drive এ অ্যাক্সেসের বিলম্বে উচ্চ-পারফর্মেন্স অ্যাপ্লিকেশনগুলির জন্য একটি মৌলিক বিষয় হতে পারে।

বিকল্প হিসেবে, ডেভেলপাররা ছোট ডেটাসেটগুলির জন্য গুগল শিট বা বড় সঞ্চয় ধারণক্ষমতা এবং উচ্চ-পারফর্মেন্স পঠন/লেখা অপারেশনগুলির দাবি করা অ্যাপ্লিকেশনগুলির জন্য Google Cloud Storage বিবেচনা করতে পারে। প্রতিটি সমাধান লেটেন্সি, সঞ্চয় সীমা এবং Google Apps Script থেকে ব্যবহারের সুবিধাজনকতা সম্পর্কে ভিন্ন ট্রেড-অফ অফার করে। অবশেষে, পছন্দটি অ্যাপ্লিকেশনের নির্দিষ্ট অনুরোধ এবং এটি যে অবকাঠামোর মধ্যে পরিচালিত হয় তার উপর নির্ভর করে।
