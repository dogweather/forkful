---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:49.534643-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP `ext-intl` \u098F\u0995\u09CD\
  \u09B8\u099F\u09C7\u09A8\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 `NumberFormatter` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09A8 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.123079-06:00'
model: gpt-4-0125-preview
summary: "PHP `ext-intl` \u098F\u0995\u09CD\u09B8\u099F\u09C7\u09A8\u09B6\u09A8\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 `NumberFormatter` \u0995\u09CD\
  \u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

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
