---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:03.400684-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u0987 \u09AC\
  \u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0995\u09C7 \u098F\u0995\u099F\u09BF \u098F\u0995\u0995 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09AE\u09BF\u09B2\u09BF\u09A4 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u09A1\u09BE\u0987\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  , \u0987\u0989\u0986\u09B0\u098F\u09B2 \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\
  \u09CB \u09B0\u09C2\u09AA\u09C7\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u2026"
lastmod: '2024-03-17T18:47:43.515186-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u0987 \u09AC\
  \u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0995\u09C7 \u098F\u0995\u099F\u09BF \u098F\u0995\u0995 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09AE\u09BF\u09B2\u09BF\u09A4 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u09A1\u09BE\u0987\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  , \u0987\u0989\u0986\u09B0\u098F\u09B2 \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\
  \u09CB \u09B0\u09C2\u09AA\u09C7\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?

স্ট্রিংগুলি যোগ করা মানে দুই বা ততোধিক স্ট্রিংকে একটি একক স্ট্রিংয়ে মিলিত করা। প্রোগ্রামাররা এটি করেন ডাইনামিকভাবে বার্তা, ইউআরএল বা যেকোনো রূপের টেক্সট তৈরি করতে যা স্থির এবং পরিবর্তনশীল কন্টেন্টের মিশ্রণ চায়।

## কিভাবে:

গুগল অ্যাপস স্ক্রিপ্টে, যা জাভাস্ক্রিপ্টের উপর ভিত্তি করে, স্ট্রিংগুলি যোগ করার বেশ কয়েকটি উপায় আছে। এখানে কিছু সাধারণ পদ্ধতি দেওয়া হল:

### প্লাস অপারেটর (`+`) ব্যবহার করে:

```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // আউটপুট: John Doe
```

### `concat()` মেথড ব্যবহার করে:

```javascript
var string1 = "Hello";
var string2 = "World";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // আউটপুট: Hello World
```

### টেমপ্লেট লিটারাল (ব্যাকটিক) ব্যবহার করে:

এটি স্ট্রিংগুলি যোগ করার একটি আধুনিক এবং নমনীয় উপায়, যা আপনাকে সহজেই স্ট্রিংয়ের মধ্যে এক্সপ্রেশনগুলি এম্বেড করতে দেয়।

```javascript
var language = "Google Apps Script";
var message = `Learning ${language} is fun!`;
Logger.log(message); // আউটপুট: Learning Google Apps Script is fun!
```

এই পদ্ধতিগুলোর প্রত্যেকের নিজস্ব ব্যবহারের ক্ষেত্র আছে, এবং তাদের মধ্যে নির্বাচন সাধারণত পড়ার সুবিধা এবং যে স্ট্রিংগুলি যোগ করা হচ্ছে তার জটিলতার উপর নির্ভর করে।

## গভীর ডুব

স্ট্রিং যোগ করা শুধু গুগল অ্যাপস স্ক্রিপ্ট নয়, অনেক প্রোগ্রামিং ভাষার একটি মৌলিক দিক। ঐতিহাসিকভাবে, স্ট্রিংগুলি যোগ করা প্লাস অপারেটর বা `concat()` এর মতো বিশেষায়িত ফাংশন/মেথড ব্যবহার করে প্রায়শই সম্পাদন করা হত। তবে, ইসিএমএস্ক্রিপ্ট 2015 (ES6) এ টেমপ্লেট লিটারালগুলির প্রবর্তনের সাথে, যা গুগল অ্যাপস স্ক্রিপ্ট সমর্থন করে, ডেভেলপাররা স্ট্রিং নিয়ে কাজ করার জন্য আরো শক্তিশালী এবং অন্তর্জ্ঞানমূলক একটি উপায় পেয়েছেন।

টেমপ্লেট লিটারালগুলি না শুধু স্ট্রিংয়ের মধ্যে এক্সপ্রেশনগুলি এম্বেড করার জন্য সিনট্যাক্স সরলীকৃত করে, বরং স্পষ্ট নিউলাইন অক্ষরের প্রয়োজন ছাড়াই মাল্টিলাইন স্ট্রিংগুলি সমর্থন করে। এটি ভুলের সম্ভাবনা কমিয়ে দেয় এবং জটিল স্ট্রিংগুলি নিয়ে কাজ করা বা একাধিক ভেরিয়েবলগুলি একটি টেক্সট টেমপ্লেটে প্রতিস্থাপন করার সময় কোডের পঠনযোগ্যতা উন্নত করে, বিশেষ করে।

যদিও `+` অপারেটর এবং `concat()` পদ্ধতি এখনও প্রচলিত এবং সহজ দৃশ্যগুলিতে সামঞ্জস্য এবং সরলতার জন্য সমর্থন করা হয়, টেমপ্লেট লিটারালগুলি স্ট্রিং যোগ করার জন্য একটি আধুনিক, প্রকাশমূলক বিকল্প অফার করে যা পঠনযোগ্যতা এবং রক্ষণাবেক্ষণ বিবেচনায় অধিকাংশ সময় উত্তম বিবেচিত হয়।

তবুও, এটি গুরুত্বপূর্ণ যে আপনার প্রকল্পের নির্দিষ্ট প্রেক্ষাপট এবং প্রয়োজনের সাথে সেরা মিলে যাওয়া পদ্ধতিটি নির্বাচন করা, যেমন টার্গেট পরিবেশের সামঞ্জস্যতা (যদিও গুগল অ্যাপস স্ক্রিপ্টের সাথে এটি কদাচিৎ একটি সমস্যা), কর্মক্ষমতা প্রভাব (বেশিরভাগ অ্যাপ্লিকেশনের জন্য ন্যূনতম), এবং আধুনিক জাভাস্ক্রিপ্ট বৈশিষ্ট্যগুলির সাথে ডেভেলপমেন্ট টিমের পরিচিতি।
