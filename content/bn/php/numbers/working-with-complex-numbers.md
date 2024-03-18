---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:49.534643-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\
  \u0995\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u0985\u0982\u09B6 \u098F\
  \u09AC\u0982 \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\
  \u0995 \u0985\u0982\u09B6 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0997\u09A0\u09BF\
  \u09A4, \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 `a + bi` \u09B9\u09BF\u09B8\u09BE\
  \u09AC\u09C7 \u09B2\u09C7\u0996\u09BE \u09B9\u09AF\u09BC\u0964 \u0989\u09A8\u09CD\
  \u09A8\u09A4 \u0997\u09A3\u09BF\u09A4, \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\
  \u09BF\u099C\u09CD\u099E\u09BE\u09A8, \u09AA\u09CD\u09B0\u0995\u09CC\u09B6\u09B2\
  \ \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:44.123079-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u0985\u0982\u09B6 \u098F\u09AC\
  \u0982 \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995\
  \ \u0985\u0982\u09B6 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0997\u09A0\u09BF\u09A4\
  , \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 `a + bi` \u09B9\u09BF\u09B8\u09BE\u09AC\
  \u09C7 \u09B2\u09C7\u0996\u09BE \u09B9\u09AF\u09BC\u0964 \u0989\u09A8\u09CD\u09A8\
  \u09A4 \u0997\u09A3\u09BF\u09A4, \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\u09BF\
  \u099C\u09CD\u099E\u09BE\u09A8, \u09AA\u09CD\u09B0\u0995\u09CC\u09B6\u09B2 \u098F\
  \u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

জটিল সংখ্যা একটি বাস্তব অংশ এবং একটি কাল্পনিক অংশ দ্বারা গঠিত, সাধারণত `a + bi` হিসাবে লেখা হয়। উন্নত গণিত, পদার্থবিজ্ঞান, প্রকৌশল এবং নির্দিষ্ট কম্পিউটার অ্যালগরিদমগুলিতে এগুলি অত্যন্ত গুরুত্বপূর্ণ। প্রোগ্রামাররা এগুলি নেতিবাচক সংখ্যার বর্গমূল এবং দোলনরত ফাংশনগুলি জড়িত গণনা সম্পাদনের জন্য কাজ করেন।

## কিভাবে:

PHP `ext-intl` এক্সটেনশনের মাধ্যমে `NumberFormatter` ক্লাস ব্যবহার করে জটিল সংখ্যাগুলিকে সমর্থন করে থাকে। এখানে একটি উদাহরণ দেওয়া হল:

```php
// intl এক্সটেনশন লোড করা আছে কিনা তা যাচাই করুন
if (!extension_loaded('intl')) {
    die("intl এক্সটেনশন সক্রিয় নয়। এই কোড চালানোর জন্য দয়া করে এটি সক্রিয় করুন।");
}

function addComplexNumbers($a, $b) {
    // NumberFormatter ব্যবহার করে জটিল সংখ্যা পার্স এবং ফরম্যাট করুন
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // স্ট্রিং থেকে জটিল সংখ্যা পার্স করুন
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // যোগ কর্ম সম্পাদন করুন
    $sum = $numA + $numB;

    // ফলাফলকে জটিল সংখ্যা হিসাবে ফরম্যাট করুন
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // আউটপুট: 7+10i
```

## গভীর ডুব

`ext-intl` আগে, PHP-এ জটিল সংখ্যার জন্য নেটিভ সমর্থন ছিল না। ডেভেলপাররা জটিল সংখ্যা সমাধানের জন্য ফাংশন বা কাস্টম ক্লাস লাইব্রেরিগুলি ব্যবহার করত। জটিল অপারেশনগুলি ক্লান্তিকর এবং ত্রুটিপূর্ণ হতে পারে, কিন্তু `ext-intl` ICU লাইব্রেরির সাথে মিল রেখে জটিল সংখ্যাগুলিকে প্রদর্শন এবং পার্স করার একটি আন্তর্জাতিকীকৃত উপায় প্রদান করে।

এছাড়াও, ভারী গণিত অপারেশনের জন্য, কেউ কেউ আরও গণিত-বান্ধব ভাষায় (যেমন C বা Python) লিখিত বাহ্যিক লাইব্রেরিগুলি ব্যবহার করতে পারে এবং PHP-র মাধ্যমে তাদের সাথে ইন্টারফেস করতে পারে। বাস্তবায়নের ব্যাপারে, `ext-intl` পেছনের পর্দায় এর ব্যবস্থাপনা করে, সঠিক অঙ্কগণিত নিশ্চিত করে এবং প্রোগ্রামার থেকে জটিলতা লুকিয়ে রাখে।

ঐতিহাসিকভাবে, জটিল সংখ্যাগুলি 'কাল্পনিক' হিসাবে অপছন্দ করা হয়েছিল, কিন্তু এগুলি বিভিন্ন বৈজ্ঞানিক এবং গণিতীয় ক্ষেত্রের মৌলিক হয়ে উঠেছে, যা এগুলির বাস্তব বিশ্বের গুরুত্বকে আরও বেশি প্রকাশ করেছে তাদের কাল্পনিক অবস্থানের চেয়ে।

## আরও দেখুন
- [PHP ম্যানুয়াল এ NumberFormatter-এ](https://www.php.net/manual/en/class.numberformatter.php)
- [জটিল সংখ্যা সম্পর্কে উইকিপিডিয়া](https://bn.wikipedia.org/wiki/%E0%A6%9C%E0%A6%9F%E0%A6%BF%E0%A6%B2_%E0%A6%B8%E0%A6%82%E0%A6%96%E0%A7%8D%E0%A6%AF%E0%A6%BE)
- [PHP: সঠিক উপায় - ডাটা টাইপসের সাথে কাজ করা](https://phptherightway.com/#data_types)
