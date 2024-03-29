---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:09.314702-06:00
description: "Google Apps Script (GAS) \u098F \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\
  \u09BE \u09B2\u09BF\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0986\u099A\
  \u09B0\u09A3 \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE\u09A4\u09C7 \u09A4\u09BE\u09B0\u09BE \u09AC\u09BF\
  \u09AD\u09BF\u09A8\u09CD\u09A8 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.528536-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script (GAS) \u098F \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u09B2\u09BF\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0986\u099A\u09B0\
  \u09A3 \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE\u09A4\u09C7 \u09A4\u09BE\u09B0\u09BE \u09AC\u09BF\
  \u09AD\u09BF\u09A8\u09CD\u09A8 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \u09A4\u09C7\u2026"
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Google Apps Script (GAS) এ পরীক্ষা লিখা মানে হচ্ছে আপনার কোডের আচরণ যাচাই করার জন্য স্বয়ংক্রিয় স্ক্রিপ্ট তৈরি করা, যাতে তারা বিভিন্ন পরিস্থিতিতে প্রত্যাশিতভাবে কাজ করে এমনটি নিশ্চিত করা যায়। প্রোগ্রামাররা এটি করে ত্রুটি দ্রুত ধরা, কোডের গুণগত মান উন্নত করা, এবং আপডেট এবং রক্ষণাবেক্ষণকে সহজ করার জন্য।

## কিভাবে:

যদিও Google Apps Script এ অন্যান্য প্রোগ্রামিং পরিবেশের মতো কোন অন্তর্নির্মিত পরীক্ষার ফ্রেমওয়ার্ক নেই, আপনি সিম্পল GAS ফাংশনের সাহায্যে বা বাহ্যিক পরীক্ষার লাইব্রেরি যেমন `QUnit` সংহত করে এখনো পরীক্ষা লিখতে এবং চালাতে পারেন। আপনার স্ক্রিপ্টের অন্য একটি ফাংশনকে পরীক্ষা করার জন্য একটি সিম্পল GAS ফাংশন ব্যবহার করে একটি মৌলিক উদাহরণ এখানে দেওয়া হলো:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test failed: add(2, 3) should be 5, but was " + result);
  } else {
    Logger.log("Test passed!");
  }
}
```

`testAdd()` চালালে, যদি `add` ফাংশন সঠিকভাবে কাজ করে তবে "Test passed!" লগ করবে, অন্যথায় একটি ত্রুটি নিক্ষেপ করবে। আরও উন্নত পদ্ধতির জন্য, Google Apps Script এর সাথে QUnit সংহত করা কিছু বাড়তি ধাপ জড়িত থাকে কিন্তু একটি শক্তিশালী পরীক্ষার পরিবেশ প্রদান করে। QUnit পরীক্ষার সেটআপের একটি নমুনা এরকম দেখায়:

1. আপনার প্রকল্পে QUnit লাইব্রেরি অন্তর্ভুক্ত করুন।
2. QUnit পরীক্ষাগুলি চালানোর জন্য একটি পরীক্ষামূলক HTML ফাইল তৈরি করুন।
3. QUnitের সিনট্যাক্স ব্যবহার করে পরীक্ষার কেসগুলি লিখুন।

QUnit ব্যবহার করে একটি উদাহরণ এরকম:

```javascript
// আপনার পরীক্ষাগুলি চালানোর জন্য ব্যবহৃত HTML ফাইলে QUnit লিঙ্ক করে অন্তর্ভুক্ত করুন

QUnit.test("Testing add function", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) এর ফলাফল ৫ হওয়া উচিত");
});
```

ফলাফলগুলি দেখতে, GAS Script Editor এর মধ্যে HTML ফাইলটি খুলুন বা এটি একটি ওয়েব অ্যাপ হিসেবে ডিপ্লয় করুন।

## গভীর ডুব

ঐতিহাসিকভাবে, Google Apps Script এ পরীক্ষাকরণ কিছুটা উপেক্ষা করা হয়েছে, সম্ভবত প্লাটফর্মের উৎপত্তি এবং প্রাথমিক ব্যবহারের ক্ষেত্রগুলি বড় অ্যাপ্লিকেশনগুলির পরিবর্তে দ্রুত, ছোট স্কেলের অটোমেশন কাজে মনোনিবেশ করার কারণে। এমনকি, GAS ঐতিহ্যবাহী প্রোগ্রামিং পরিবেশগুলিতে পাওয়া একই রকম দৃঢ় পরীক্ষার ফ্রেমওয়ার্ক এবং টুলস অফার করে না। তবে, কমিউনিটি খোলা সোর্স লাইব্রেরি অন্তর্ভুক্ত করে এবং Google এর বিদ্যমান টুলগুলি সৃজনশীলভাবে ব্যবহার করে খাপ খাইয়ে নিচ্ছে।

QUnit এর মতো লাইব্রেরি ব্যবহার করা একটি বড় প্রগতি হলেও এর নিজের সেট চ্যালেঞ্জ এনেছে, যেমন একটি উপযুক্ত পরীক্ষার পরিবেশ সেটআপ করা এবং একটি অতিরিক্ত সিনট্যাক্স শিখতে হবে। তবে, GAS এর সাথে আরও জটিল এবং নির্ভরযোগ্য অ্যাপ্লিকেশন তৈরি করতে আগ্রহীদের জন্য প্রচেষ্টাটি মূল্যবান।

পরীক্ষার জন্য সিম্পল GAS ফাংশনগুলি ব্যবহার করা সহজ ব্যবহার এবং GAS পরিবেশের সাথে সংশ্লেষ অফার করে কিন্তু ব্যাপক পরীক্ষার বৈশিষ্ট্য এবং আপনার প্রকল্পের বৃদ্ধির সাথে সহজে স্কেল করা এবং কম্প্রিহেনসিভ সাপোর্টের অভাব রয়েছে। ক্ল্যাস্প (Google Apps Script Command Line Interface) এর মতো টুলগুলি পরীক্ষাসহ আরও উন্নত কাজপ্রবাহ সহায়তা করতে পারে, ডেভেলপারদের তাদের পছন্দের IDE তে কোড করতে দেয়, বাহ্যিক পরীক্ষার ফ্রেমওয়ার্কগুলির সাথে আরও সুষ্ঠুভাবে সংযোগ করার সুযোগ প্রদান করে।

সংক্ষেপে, যদিও GAS সোজাসাপটা সমর্থন না থাকলেও উন্নত পরীক্ষার জন্য, এর নমনীয়তা এবং কমিউনিটির উদ্ভাবনী পদ্ধতিগুলি আপনার স্ক্রিপ্টগুলি যেন দৃঢ়, নির্ভরযোগ্য, এবং যেকোনো কাজের জন্য প্রস্তুত থাকার জন্য বাস্তবসম্মত পথ প্রদান করে।
