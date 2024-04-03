---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:33.281768-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script-\u098F \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\
  \u09CB\u09DF\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\
  \u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B9\
  \u09AF\u09BC, \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09BF\u0982\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7 \u0989\u09AA\u09B2\u09AC\u09CD\u09A7\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 JavaScript \u09AE\u09C7\u09A5\u09A1\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09CC\u099C\u09A8\u09CD\u09AF\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:43.509149-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script-\u098F \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0 \u0995\u09C7\u09B8\
  \u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF \u09B9\u09AF\u09BC, \u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7\
  \ \u0989\u09AA\u09B2\u09AC\u09CD\u09A7 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ JavaScript \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09CC\
  \u099C\u09A8\u09CD\u09AF\u09C7\u0964 `toLowerCase()` \u09AE\u09C7\u09A5\u09A1\u099F\
  \u09BF \u0986\u09AA\u09A8\u09BF \u09AC\u09C7\u09B6\u09BF\u09B0\u09AD\u09BE\u0997\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\
  \u09AC\u09C7 \u098F\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\
  \u09BC\u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Google Apps Script-এ একটি স্ট্রিংকে লোয়ার কেসে রূপান্তর করা সরাসরি হয়, স্ক্রিপ্টিং পরিবেশে উপলব্ধ বিল্ট-ইন JavaScript মেথডগুলির সৌজন্যে। `toLowerCase()` মেথডটি আপনি বেশিরভাগ ব্যবহার করবেন। এখানে আপনি কিভাবে এটি বাস্তবায়ন করতে পারেন:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // আউটপুট: hello, world!
}
```

এই সাধারণ ফাংশনটি একটি মৌলিক স্ট্রিং নিয়ে, `toLowerCase()` মেথড প্রয়োগ করে, এবং ফলাফল লগ করে। যখন আমরা সেই ইনপুটগুলির সাথে কাজ করি যা কেস-ইনসেনসিটিভ হতে প্রয়োজন, তখন এটি বিশেষভাবে উপকারী। উদাহরণ স্বরূপ, বিভিন্ন কেসে ইউজারদের ইনপুটের ই-মেইল ঠিকানাগুলি তুলনা করা।

অতিরিক্তভাবে, যখন আপনি অ্যারে ডেটার সাথে কাজ করেন, প্রতিটি উপাদানকে লোয়ার কেসে রূপান্তর করার জন্য আপনি তাদের মাধ্যমে ম্যাপ করতে পারেন:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // আউটপুট: [alice, bob, charlie]
}
```

এই উদাহরণটি যখন স্ট্রিং ডেটার বহুগুলি নিয়ে কাজ করে তখন `toLowerCase()`-এর বহুমুখিতা জোর দেয়, যা আপনার ডেটাসেট জুড়ে ঐক্যমত্য নিশ্চিত করে।

## গভীরে যাওয়া
`toLowerCase()` মেথডটি, JavaScript থেকে উত্তরাধিকার সূত্রে পাওয়া এবং Google Apps Script-এ ব্যবহৃত, টেক্সটুয়াল ডেটার কেস-ইনসেনসিটিভ হ্যান্ডলিং সাহায্যের জন্য প্রাথমিক JavaScript সংস্করণগুলি থেকে স্ট্রিং পরিচালনার একটি অবিচ্ছেদ্য অংশ হয়ে ওঠে। তার সরলতা সত্ত্বেও, এই প্রক্রিয়াটি ডেটা সত্যায়ন, সাজানো, এবং অনুসন্ধান অ্যালগরিদমে কেস সেনসিটিভিটির কারণে উদ্ভূত জটিলতা হ্রাস করে একটি প্রধান ভূমিকা রাখে।

পারফরম্যান্সের দিক দিয়ে, রূপান্তর প্রক্রিয়া আধুনিক JavaScript ইঞ্জিনগুলিতে অত্যন্ত অপ্টিমাইজড; তবে, ব্যাপক স্কেলের ডেটা অপারেশনে অপ্রয়োজনীয় প্রক্রিয়াজনিত ওভারহেড এড়ানোর জন্য এর প্রয়োগ এখনও যথাযথ হওয়া উচিত।

বিশেষত যখন জটিল প্যাটার্নগুলির সাথে কাজ করা হয় অথবা লোকেল-নির্দিষ্ট রূপান্তরের প্রয়োজন হয়, তখন বিবেচনায় নেওয়ার জন্য একটি বিকল্প হ'ল `toLocaleLowerCase()` মেথড। এই বৈকল্পিক পদ্ধতিটি অক্ষরগুলিকে লোয়ার কেসে রূপান্তরের জন্য লোকেল-নির্দিষ্ট নিয়মগুলি বিবেচনা করে, যা বহুভাষিক অ্যাপ্লিকেশনগুলি সমর্থন করার জন্য অপরিহার্য হতে পারে:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // আউটপুট: märz
```

অতিরিক্ত জটিলতা সত্ত্বেও, `toLocaleLowerCase()` আন্তর্জাতিক অ্যাপ্লিকেশনগুলির জন্য একটি শক্তিশালী সরঞ্জাম, যা নিশ্চিত করে যে রূপান্তরটি ব্যবহারকারীর লোকেলের ভাষাগত মানদণ্ডগুলি মেনে চলে। আপনি যে পদ্ধতিটি বেছে নিন না কেন, Google Apps Script-এ স্ট্রিংগুলিকে লোয়ার কেসে রূপান্তর করা টেক্সট প্রসেসিংয়ের একটি অপরিহার্য অংশ হিসেবে থাকে, ইউজার ইনপুট এবং মানকিকৃত ডেটা হ্যান্ডলিংয়ের মধ্যকার ফাঁকটি পূরণ করে।
