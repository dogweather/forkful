---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:52.467536-06:00
description: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\
  \ \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09AE\u09C7\u09A7\u09BE\u09AC\u09C0 \u099A\u09BF\u09A8\u09CD\
  \u09A4\u09BE\u09AD\u09BE\u09AC\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u0995\u09CB\u09A1\u09AC\u09C7\u09B8\
  \ \u09B8\u09C7\u099F\u0986\u09AA \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09A7\
  \u09BE\u09B0\u09A3\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\u09BE\u09B8\u09CD\
  \u09A4\u09AC, \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2 \u0985\u09CD\u09AF\u09BE\
  \u09AA \u09AC\u09BE \u09B8\u09C7\u09AC\u09BE\u09DF\u2026"
lastmod: '2024-03-17T18:47:44.453950-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AE\u09C7\u09A7\u09BE\u09AC\u09C0 \u099A\u09BF\u09A8\u09CD\u09A4\
  \u09BE\u09AD\u09BE\u09AC\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u0995\u09CB\u09A1\u09AC\u09C7\u09B8 \u09B8\
  \u09C7\u099F\u0986\u09AA \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09A7\u09BE\
  \u09B0\u09A3\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\
  \u09AC, \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2 \u0985\u09CD\u09AF\u09BE\u09AA\
  \ \u09AC\u09BE \u09B8\u09C7\u09AC\u09BE\u09DF\u2026"
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

নতুন প্রকল্প শুরু করা মানে আপনার মেধাবী চিন্তাভাবনার জন্য একটি নতুন কোডবেস সেটআপ করা। প্রোগ্রামাররা এটি ধারণাগুলিকে বাস্তব, ফাংশনাল অ্যাপ বা সেবায় রূপান্তরিত করতে করে থাকেন।

## কিভাবে:

কোড লেখার আগে, টুল এবং কাঠামো নির্ধারণ করুন। চলুন এই উদাহরণের জন্য Node.js এবং npm (Node Package Manager) ব্যবহার করি।

১. [অফিসিয়াল ওয়েবসাইট](https://nodejs.org/) থেকে Node.js ইনস্টল করুন।
২. একটি টার্মিনাল খুলুন এবং চালান:

```javascript
npm init
```

সেটআপ প্রশ্নগুলোর উত্তর দিন। বুম—`package.json` তৈরি হয়ে যায়, যা আপনার প্রকল্পকে বর্ণনা করে। পরবর্তীতে, আসুন জনপ্রিয় ওয়েব ফ্রেমওয়ার্ক Express যুক্ত করি:

```javascript
npm install express --save
```

এখন, `index.js` এ একটি সিম্পল ওয়েব সার্ভার লিখুন:

```javascript
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('Hello World!');
});

app.listen(3000, () => {
  console.log('Server is up on port 3000');
});
```

আপনার সার্ভার চালান:

```javascript
node index.js
```

নমুনা আউটপুট:

```
Server is up on port 3000
```

আপনার ওয়েব ব্রাউজারে `http://localhost:3000` এ নেভিগেট করুন। আপনি "Hello World!" দেখতে পাবেন।

## বিশদ আলোচনা

ঐতিহাসিকভাবে, প্রকল্প সেটআপ একটি ব্যথার জায়গা ছিল, প্রচুর ম্যানুয়াল কনফিগারেশনের সাথে। বর্তমানে, npm এর মতো টুলগুলি ভারী কাজ করে। ফ্রন্ট-এন্ড প্রকল্পের জন্য, `create-react-app` বা `vue-cli` বিবেচনা করুন। Node.js এর জন্য, Express একটি দৃঢ় পছন্দ, শক্তি এবং সাধারণতার সাথে ভারসাম্য রেখে। এটি হালকা কিন্তু বেশিরভাগ ওয়েব সার্ভারের প্রয়োজনের জন্য শক্তিশালী বৈশিষ্ট্য সম্পন্ন।

মনে রাখবেন, আপনার প্রকল্প কীভাবে সংগঠিত করা হয় তা গুরুত্বপূর্ণ। পারম্পরিক Node.js অ্যাপগুলিতে একটি এন্ট্রি পয়েন্ট (যেমন `index.js`), নির্ভরতা পরিচালনা করার জন্য একটি `package.json` ফাইল এবং উদ্বেগ পৃথকীকরণের জন্য একটি ফোল্ডার কাঠামো (মডিউল, ইউটিলিটি, রুট, ইত্যাদি) থাকে।

প্যাকেজ ম্যানেজমেন্টের জন্য npm এর বিকল্পগুলির মধ্যে Yarn রয়েছে, যা গতি এবং ধারাবাহিকতা উন্নতি অফার করে। প্রকল্প স্ক্যাফোল্ডিং এর জন্য, Yeoman অনেক ধরণের প্রকল্প এবং প্রযুক্তির জেনারেটর সরবরাহ করে সাহায্য করে।

## দেখুন

- Node.js [নথি](https://nodejs.org/en/docs/)
- Express [অফিসিয়াল সাইট](https://expressjs.com/)
- `create-react-app` [GitHub রিপো](https://github.com/facebook/create-react-app)
- Vue CLI [নথি](https://cli.vuejs.org/)
- Yarn [অফিসিয়াল সাইট](https://classic.yarnpkg.com/lang/en/)
- Yeoman [অফিসিয়াল সাইট](http://yeoman.io/)
