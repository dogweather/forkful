---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:25.192809-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u09C7\u09B0 Fetch API \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983."
lastmod: '2024-03-17T18:47:44.452938-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 Fetch API \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2\u0983."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
এখানে জাভাস্ক্রিপ্টের Fetch API ব্যবহার করে একটি দ্রুত উদাহরণ দেওয়া হলঃ

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Network response was not ok.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch error: ', error));
```

নমুনা আউটপুট (কনসোলে প্রিন্ট করা হয়েছে):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## গভীর আলোচনা
ডুব দেওয়ার আগে, একটু প্রেক্ষাপট পাওয়া যাক। মৌলিক প্রমাণীকরণ ওয়েব সেবা নিরাপত্তার অন্যতম সরল রূপ, প্রতিটি অনুরোধের সাথে শিরোনামের মাধ্যমে প্রমাণপত্র পাঠানো।

ইতিহাস প্রসঙ্গ:
- মৌলিক HTTP প্রমাণীকরণ একটি পুরানো পদ্ধতি, প্রথমে 2015 সালে RFC 7617-এ বর্ণনা করা হয়েছিল, যা আরও পুরানো RFC 2617 এর 1999 সালে প্রতিস্থাপিত হয়েছিল।
- এর সরলতার কারণে এটি ব্যাপকভাবে ব্যবহৃত হতো তবে HTTPS ছাড়া এটি নিরাপদ নয়, কারণ বেস64 কোডিং সহজেই বিপরীতমুখী করা যায়।

বিকল্প:
- OAuth: প্রবেশাধিকার প্রদানের জন্য একটি আরও নিরাপদ এবং জটিল মানদণ্ড, যখন আপনার পাসওয়ার্ড প্রমাণপত্র ভাগ না করে প্রবেশাধিকার প্রদান প্রয়োজন।
- API কীস: জটিল OAuth প্রোটোকলের চেয়ে পরিচালনা করা সহজ একটি একক টোকেন।
- বিয়ারার টোকেনস: বিশেষ করে JWT (JSON ওয়েব টোকেনস), যা আরও তথ্য বহন করতে পারে।

বাস্তবায়ন বিস্তারিত:
- বেস64 কোডিং ব্যবহারকারীর নাম:পাসওয়ার্ড স্ট্রিংকে একটি বেশি বিশ্বব্যাপী প্রেরণীয় অক্ষরের ক্রমে রূপান্তর করে।
- সবসময় নিশ্চিত করুন যে সংযোগটি HTTPS এ হয়, যাতে প্রমাণপত্রগুলি অন্যথা আটকানো হতে পারে।
- আধুনিক উন্নয়নে, প্রমাণীকরণের জন্য টোকেন এবং সেশন কুকিজ পছন্দ করা হয়, কারণ এগুলি আরও নিরাপদ এবং বহুমুখী।

## আরো দেখুন
- [মোজিলা ডেভেলপার নেটওয়ার্ক - প্রমাণীকরণ](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP মৌলিক প্রমাণীকরণ](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0 এর পরিচিতি](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [JSON ওয়েব টোকেনস (JWT)](https://jwt.io/introduction/)
