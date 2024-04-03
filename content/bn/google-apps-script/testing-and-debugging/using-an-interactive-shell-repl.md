---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:07.891079-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script, \u09AF\u09BE\
  \ Google \u09AA\u09A3\u09CD\u09AF\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u0995\u09BE\
  \u099C \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u0995\
  \u09CD\u09B2\u09BE\u0989\u09A1-\u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\
  , Python \u09AC\u09BE JavaScript's Node.js \u098F\u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-03-17T18:47:43.526267-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u09AF\u09BE Google \u09AA\u09A3\u09CD\u09AF\u0997\u09C1\
  \u09B2\u09BF\u09A4\u09C7 \u0995\u09BE\u099C \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\
  \u0995\u09CD\u09B0\u09BF\u09AF\u09BC \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u0995\u099F\u09BF \u0995\u09CD\u09B2\u09BE\u0989\u09A1-\u09AD\u09BF\
  \u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u09BF\u0982 \u09AD\u09BE\u09B7\u09BE, Python \u09AC\u09BE JavaScript's Node.js\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09BF\
  \u09A4\u09C7 \u09A5\u09BE\u0995\u09BE \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\
  \ REPL \u099F\u09C1\u09B2\u09C7\u09B0 \u09AE\u09A4\u09CB \u098F\u0995\u099F\u09BF\
  \ \u09A8\u09BE \u09A5\u09BE\u0995\u09B2\u09C7\u0993, \u0986\u09AA\u09A8\u09BF Apps\
  \ Script \u098F\u09A1\u09BF\u099F\u09B0\u09C7\u09B0 \u09B2\u0997\u09BF\u0982 \u098F\
  \u09AC\u0982 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u09AC\u09C8\u09B6\u09BF\
  \u09B7\u09CD\u099F\u09CD\u09AF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09AC\u09BE \u098F\u0995\u099F\u09BF \u09AC\u09BE\
  \u09B9\u09CD\u09AF\u09BF\u0995 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6 \u09B8\u09C7\
  \u099F \u0986\u09AA \u0995\u09B0\u09C7 \u098F\u0995\u0987 \u09B0\u0995\u09AE \u0985\
  \u09AD\u09BF\u099C\u09CD\u099E\u09A4\u09BE \u0985\u09A8\u09C1\u0995\u09B0\u09A3\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7, \u0986\u09AE\u09B0\u09BE Apps Script \u098F\u09A1\u09BF\u099F\u09B0\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09B8\
  \u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 REPL \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \u09B0 \u0989\u09AA\u09B0 \u09AE\u09A8\u09CB\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\
  \u09B0\u09AC\u0964\n\n1."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
Google Apps Script, যা Google পণ্যগুলিতে কাজ স্বয়ংক্রিয় করার জন্য একটি ক্লাউড-ভিত্তিক স্ক্রিপ্টিং ভাষা, Python বা JavaScript's Node.js এর মতো ভাষাগুলিতে থাকা নির্মিত REPL টুলের মতো একটি না থাকলেও, আপনি Apps Script এডিটরের লগিং এবং ডিবাগিং বৈশিষ্ট্যগুলি ব্যবহার করে বা একটি বাহ্যিক পরিবেশ সেট আপ করে একই রকম অভিজ্ঞতা অনুকরণ করতে পারেন। এখানে, আমরা Apps Script এডিটরের মধ্যে একটি অস্থায়ী REPL তৈরি করার উপর মনোনিবেশ করব।

1. **একটি অস্থায়ী REPL ফাংশন তৈরি**:

```javascript
function myREPL() {
  var input = Logger.log('Enter your expression: ');
  try {
    var result = eval(input);
    Logger.log('Result: ' + result);
  } catch(e) {
    Logger.log('Error: ' + e.message);
  }
}
```

Apps Script পরিবেশে প্রথাগত REPL এর মতো সরাসরি ব্যবহারকারী ইনপুট সম্ভব না হওয়ায়, আপনি `input` ভেরিয়েবলটি ম্যানুয়ালি পরিবর্তন করতে পারেন এবং প্রেস পরীক্ষা করার জন্য `myREPL()` চালাতে পারেন।

2. **নমুনা কোড এক্সিকিউশন**:

ধরুন আপনি `2+2` মূল্যায়ন করতে ইচ্ছুক। আপনি `myREPL` ফাংশনটি নিম্নলিখিতভাবে পরিবর্তন করবেন:

```javascript
function myREPL() {
  var input = '2+2'; // এখানে ম্যানুয়ালি আপনার এক্সপ্রেশন লিখুন
  // বাকি যেমন আছে তেমনই থাকবে...
}
```

`myREPL()` চালানোর পর, লগ (View > Logs) চেক করুন, যেখানে আউটপুট নিম্নরূপ পাবেন:

```
[20-xx-xxxx xx:xx:xx:xxx] Enter your expression:
[20-xx-xxxx xx:xx:xx:xxx] Result: 4
```

3. **Logger এর সাথে ডিবাগিং**:

আরও জটিল ডিবাগিং এর জন্য, আপনার কোডের মধ্যে `Logger.log(variable);` ব্যবহার করে ভেরিয়েবল স্টেটগুলি প্রিন্ট করুন, যা আপনাকে আপনার স্ক্রিপ্টের প্রবাহ এবং মধ্যবর্তী অবস্থাগুলি বুঝতে সাহায্য করবে।

## গভীর ডুব
REPL এর ধারণাটি কম্পিউটিং এর ইতিহাসে গভীরভাবে মূলগত, ১৯৬০ এর দশকের টাইম-শেয়ারিং সিস্টেমগুলি থেকে আসা, যা ইন্টার‌্যাক্টিভ সেশনগুলির জন্য সম্ভব করেছে। Lisp এর মতো ভাষাগুলি এই পরিবেশে সফল হয়েছিল, কারণ REPL তাদের পুনরাবৃত্তিমূলক উন্নয়ন প্রক্�
