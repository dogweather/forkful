---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:37.138235-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u099C\u099F\u09BF\u09B2\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\
  \u09B2\u09CD\u09A1-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\
  \u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B9\u09BE\u09A4\u09C7\u09B0 \u09B8\u09CD\u09B2\u09BF\u09AD \u0997\u09C1\
  \u099F\u09BF\u09AF\u09BC\u09C7 \u09AC\u09B8\u09CD\u09A4\u09C1 \u098F\u09AC\u0982\
  \ \u0997\u09A3\u09BF\u09A4\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u098F\u09B0 \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0\
  \u2026"
lastmod: '2024-03-17T18:47:44.446636-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u09A1-\u0987\u09A8 \u09B8\u09BE\
  \u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\
  \u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u09B9\u09BE\u09A4\u09C7\u09B0 \u09B8\
  \u09CD\u09B2\u09BF\u09AD \u0997\u09C1\u099F\u09BF\u09AF\u09BC\u09C7 \u09AC\u09B8\
  \u09CD\u09A4\u09C1 \u098F\u09AC\u0982 \u0997\u09A3\u09BF\u09A4\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u09B0 \u09B8\u09AE\u09BE\u09A7\u09BE\
  \u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u09A6\u09C3\u09B7\u09CD\u099F\u09BE\u09A8\u09CD\u09A4 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
জাভাস্ক্রিপ্টে জটিল সংখ্যার জন্য বিল্ড-ইন সাপোর্ট নেই, তবে আপনি আপনার হাতের স্লিভ গুটিয়ে বস্তু এবং গণিতের মাধ্যমে এর সমাধান করতে পারেন। এখানে তার একটি দ্রুত দৃষ্টান্ত দেওয়া হল।

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...প্রয়োজন অনুযায়ী আরও মেথড (বিয়োগ, গুণ, ভাগ) যোগ করুন

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Result: ${result}`); // ফলাফল: 4 + 6i
```

## গভীরে ডুব:
জটিল সংখ্যা 16শ শতাব্দী থেকে প্রচলিত, ধন্যবাদ ইটালিয়ান গণিতবিদ Gerolamo Cardano কে। তারা প্রকৌশল এবং পদার্থ বিজ্ঞানের মতো বিভিন্ন ক্ষেত্রে অপরিহার্য হয়ে ওঠে। আধুনিক প্রোগ্রামিংয়ে, তারা সিমুলেশন এবং বহু-মাত্রিক প্রয়োজন সহ এলগোরিদমের জন্য চাবিকাঠি।

এখন, জাভাস্ক্রিপ্ট জটিল সংখ্যার জন্য প্রস্তুত নয় নেটিভভাবে। কিন্তু DIY অপশনের বাইরে, আপনি math.js বা numeric.js এর মতো গণিত লাইব্রেরি ব্যবহার করতে পারেন। এগুলি জটিল সংখ্যা তোলার জন্য শক্তি প্রদানের পাশাপাশি অধিক অপারেশন, ম্যাগনিটিউড গণনা, এবং যুক্তি খোঁজার মতো অতিরিক্ত সুবিধা দেয়।

এর আড়ালে যখন আপনি জটিল সংখ্যার সাথে অপারেশন করেন, এটা দুইটি আলাদা সংখ্যাকে একত্রে ব্যবহার করার মতো। যোগ এবং বিয়োগ সরাসরি খেলা—বাস্তবকে বাস্তবের সাথে, কাল্পনিককে কাল্পনিকের সাথে মেলান। গুণ ও ভাগ আন্ত:পদ নাচের সাথে আরও মশলাদার হয়ে ওঠে এবং আরও যত্ন প্রয়োজন হয়।

## আরও দেখুন
- জাভাস্ক্রিপ্টের উপর MDN ওয়েব ডকস: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- জটিল সংখ্যাসমূহ সম্বলিত একটি গণিত লাইব্রেরি, Math.js: https://mathjs.org/docs/datatypes/complex_numbers.html
- আরেকটি লাইব্রেরি, Numeric.js: http://numericjs.com/documentation.html
- জটিল সংখ্যা সম্পর্কে গভীর ডাইভ (গণিত কেন্দ্রিক): https://mathworld.wolfram.com/ComplexNumber.html
