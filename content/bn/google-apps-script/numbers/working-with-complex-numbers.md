---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:40.872574-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u099C\u099F\
  \u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09A8 \u0995\u09B0\u09C7 \u09A8\u09BE, \u09AF\u09BE\u09B0 \u09AB\u09B2\u09C7 \u0995\
  \u09BE\u09B8\u09CD\u099F\u09AE \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\
  \u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\u09BC\u09A8 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964 \u09A8\u09BF\u099A\u09C7 \u099C\u099F\
  \u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B8\u09AE\u09CD\u09AA\u09B0\
  \u09CD\u0995\u09C7 \u09AF\u09CB\u0997, \u09AC\u09BF\u09AF\u09BC\u09CB\u0997, \u098F\
  \u09AC\u0982 \u0997\u09C1\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.517344-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7 \u09A8\u09BE, \u09AF\
  \u09BE\u09B0 \u09AB\u09B2\u09C7 \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AB\u09BE\
  \u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \u09BE\u09AF\u09BC\u09A8 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964\
  \ \u09A8\u09BF\u099A\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09C7 \u09AF\u09CB\u0997, \u09AC\
  \u09BF\u09AF\u09BC\u09CB\u0997, \u098F\u09AC\u0982 \u0997\u09C1\u09A3 \u09B8\u09AE\
  \u09CD\u09AA\u09BE\u09A6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0995\u09BE\u09A0\u09BE\u09AE\u09CB\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Google Apps Script জটিল সংখ্যা সমর্থন করে না, যার ফলে কাস্টম ফাংশনালিটি বাস্তবায়ন প্রয়োজন। নিচে জটিল সংখ্যা সম্পর্কে যোগ, বিয়োগ, এবং গুণ সম্পাদনের জন্য একটি মৌলিক কাঠামো দেওয়া হলো।

```javascript
// জটিল সংখ্যার জন্য একটি কন্সট্রাক্টর সংজ্ঞায়িত করুন
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// দুটি জটিল সংখ্যা যোগ করার মেথড
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// দুটি জটিল সংখ্যা বিয়োগ করার মেথড
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// দুটি জটিল সংখ্যা গুণ করার মেথড
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// উদাহরণ ব্যবহার
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// দুটি জটিল সংখ্যা যোগ
var sum = num1.add(num2);
console.log(`Sum: ${sum.real} + ${sum.imag}i`); // Sum: 4 + 6i

// দুটি জটিল সংখ্যা বিয়োগ
var difference = num1.subtract(num2);
console.log(`Difference: ${difference.real} + ${difference.imag}i`); // Difference: 2 + 2i

// দুটি জটিল সংখ্যা গুণ
var product = num1.multiply(num2);
console.log(`Product: ${product.real} + ${product.imag}i`); // Product: -5 + 10i
```

## গভীর ডুব:
জটিল সংখ্যার ধারণা ১৬তম শতাব্দীতে ফিরে গেলেও, এটি ইউলার এবং গাউসের মত গণিতবিদদের কাজ যা তাদের গণিতে একটি স্থায়ী স্থান দিয়েছে। তাদের উপযোগিতা সত্ত্বেও, জাভাস্ক্রিপ্ট বা Google Apps Script-এ সরাসরি জটিল সংখ্যা সমর্থন করা হয় না। সরাসরি সমর্থনের অভাব মানে জটিল সংখ্যার উপর অপারেশনগুলি ম্যানুয়ালি বাস্তবায়ন করতে হবে, যেমন দেখানো হয়েছে। যদিও এটি একটি ভাল শিক্ষা সুযোগ এবং মৌলিক প্রয়োজনের জন্য যথেষ্ট ফাংশনালিটি প্রদান করে, জটিল সংখ্যাগুলির উপর ভারী গণনা কাজের জন্য, কেউ গণিতিক গণনার জন্য আরও উপযুক্ত প্রোগ্রামিং পরিবেশের দিকে ঝুঁকতে পারে, যেমন Python সহ NumPy, যা জটিল সংখ্যা সমালোচনার জন্য অন্তর্নির্মিত, অত্যন্ত অপ্টিমাইজড অপারেশন অফার করে। তবে, Google Apps Script-এ মৌলিক অপারেশন বুঝতে এবং বাস্তবায়ন করা একটি বিস্তৃত প্রেক্ষাপটে তাদের প্রোগ্রামিং দক্ষতা বাড়ানো এবং প্রয়োগ করার জন্য একটি উপযোগী অনুশীলন।
