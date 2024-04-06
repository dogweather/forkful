---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:40:08.399607-07:00
description: null
lastmod: '2024-04-05T21:53:52.271581-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

# কিভাবে:
Elm ব্রাউজারে চালানো হয়, তাই এটির সরাসরি ফাইলসিস্টেমে অ্যাক্সেস নেই। তাই, আপনি পারম্পরিক অস্থায়ী ফাইল তৈরি করতে পারবেন না। কিন্তু, যদি আপনার এরকম একটা বৈশিষ্ট্যের প্রয়োজন হয়, আমরা Elm পোর্টস ব্যবহার করে JavaScript এর সাথে ইন্টার‍্যাক্ট করি, যা অস্থায়ী ফাইল তৈরি করতে পারে।

```elm
port module Main exposing (..)

-- JavaScript এ একটি অস্থায়ী ফাইল তৈরির জন্য একটি পোর্ট নির্ধারণ করুন 
port createTempFile : String -> Cmd msg

--একটি অস্থায়ী ফাইল তৈরি করার জন্য ডেটা JavaScript এ পাঠান 
saveDataTemporarily : String -> Cmd msg
saveDataTemporarily data =
    createTempFile data
```

JavaScript অংশের জন্য, File API ব্যবহার করে:

```javascript
app.ports.createTempFile.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var url = URL.createObjectURL(blob);

    // এখানে আপনি ইউআরএল ব্যবহার করে ব্লব ডাউনলোড করতে বা আপনার অ্যাপের অন্যান্য অংশকে পাস করতে পারেন
    console.log(url);  // এটি অস্থায়ী ফাইলের URL লগ করে 
});
```

JavaScript কনসোলে নমুনা আউটপুট:

```plaintext
blob:null/2135a9b7-1aad-4e7a-8bce-19c4f3f6d7ff
```

# গভীর নজর
Elm নিরাপদ এবং বিশ্বস্ত হতে সক্ষম হওয়ার জন্য নকশা করা হয়েছে, তাই সরাসরি ফাইল সিস্টেম অ্যাক্সেস নেই। পরিবর্তে, Elm পোর্টস ব্যবহার করে JavaScript এর সাথে যোগাযোগ করে, যা অস্থায়ী ফাইল তৈরির মতো অপারেশনগুলির জন্য অনুমতি দেয়। ঐতিহাসিকভাবে, আমরা ব্রাউজারে ফাইল-ভিত্তিক কাজগুলি JavaScript APIs ব্যবহার করে হ্যান্ডেল করে থাকি, টাইপ-সেইফ, উচ্চ-স্তরের যুক্তির জন্য Elm ব্যবহার করে।

ভবিষ্যতে, WebAssembly এর মতো বিকল্পগুলি হয়তো ভবিষ্যতে আরও সরাসরি ফাইলসিস্টেম ইন্টার‍্যাকশন অনুমতি দেবে, কিন্তু এখন, JavaScript এর সাথে ইন্টারঅপের প্রথা মেনে চলা হয়।

বাস্তবায়নের দিক দিয়ে দেখলে, ব্রাউজারের প্রেক্ষাপটে অস্থায়ী ফাইল তৈরি মানে ফাইলসিস্টেমে আসল ফাইল নয়, বরং একটি ইন-মেমোরি প্রতিনিধিত্ব (ব্লব) যা আপনি প্রয়োজন মতো কাজে লাগাতে এবং সংরক্ষণ করতে পারেন।

# আরো দেখুন
- [Elm পোর্টস](https://guide.elm-lang.org/interop/ports.html)
- [MDN - ওয়েব APIs - ফাইল](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [MDN - ওয়েব APIs - ব্লব](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
