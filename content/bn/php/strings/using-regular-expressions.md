---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:20.471517-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP PCRE (Perl Compatible Regular\
  \ Expressions) \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4\
  \ \u09AA\u09CD\u09B0\u0995\u09BE\u09B6 (regex) \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE \u0985\u09A8\u09C7\u0995 \u09AC\u09BF\u099A\u09BF\
  \u09A4\u09CD\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8 \u0985\u09AB\u09BE\u09B0 \u0995\
  \u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \u2026"
lastmod: '2024-03-17T18:47:44.118812-06:00'
model: gpt-4-0125-preview
summary: "PHP PCRE (Perl Compatible Regular Expressions) \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A8\
  \u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u09AA\u09CD\u09B0\u0995\u09BE\u09B6 (regex)\
  \ \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE \u0985\u09A8\
  \u09C7\u0995 \u09AC\u09BF\u099A\u09BF\u09A4\u09CD\u09B0 \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u0985\u09AB\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09B2."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
PHP PCRE (Perl Compatible Regular Expressions) লাইব্রেরির মাধ্যমে নিয়মিত প্রকাশ (regex) সমর্থন করে, যা অনেক বিচিত্র ফাংশন অফার করে। এখানে তাদের ব্যবহার করার উপায় দেওয়া হল:

### একটি প্যাটার্ন মিলান:
যদি কোনো স্ট্রিংয়ের মধ্যে একটা প্যাটার্ন আছে কিনা তা যাচাই করতে, `preg_match()` ব্যবহার করুন। যদি স্ট্রিংয়ের মধ্যে প্যাটার্নটি পাওয়া যায়, তবে এই ফাংশন 1 রিটার্ন করে এবং যদি না পাওয়া যায় তবে 0 রিটার্ন করে।

```php
if (preg_match("/\bweb\b/i", "PHP is a web scripting language")) {
    echo "একটি মিল পাওয়া গিয়েছে।";
} else {
    echo "কোনো মিল পাওয়া যায়নি।";
}
// আউটপুট: একটি মিল পাওয়া গিয়েছে।
```

### সকল মিল খুঁজে পাওয়া:
`preg_match_all()` তখন ব্যবহৃত হয় যখন আপনার একটি স্ট্রিংয়ের মধ্যে একটি প্যাটার্নের সকল ঘটনা খুঁজে পাওয়ার প্রয়োজন হয়।

```php
$text = "cats and dogs";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// আউটপুট: Array ( [0] => cats [1] => and [2] => dogs )
```

### টেক্সট প্রতিস্থাপন:
যে টেক্সটের সাথে নিয়মিত প্রকাশন (regex) মাচ করে সেই টেক্সট প্রতিস্থাপনের জন্য `preg_replace()` ব্যবহৃত হয়। এটি ডেটা ফর্ম্যাটিং এবং পরিষ্কার করার জন্য অসাধারণভাবে শক্তিশালী।

```php
$originalText = "April 15, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// আউটপুট: April1,2003
```

### স্ট্রিংস বিভাজন:
আপনি একটি স্ট্রিংকে অ্যারে হিসেবে বিভাজন করতে পারেন `preg_split()` ব্যবহার করে, যেখানে ডেলিমিটারের জন্য একটি প্যাটার্ন নির্দিষ্ট করা হয়।

```php
$text = "PHP is, an extremely popular, scripting language";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// আউটপুট: Array ( [0] => PHP is [1] => an extremely popular [2] => scripting language )
```

তাছাড়া, জটিল regex প্যাটার্ন এবং কাজের জন্য, Symfony’s `Finder` কম্পোনেন্ট বা Laravel-এর সহায়ক ফাংশনগুলির মত ফ্রেমওয়ার্ক এবং লাইব্রেরিগুলি আরও সুবিধাজনক অ্যাবস্ট্রাকশন লেয়ার সরবরাহ করতে পারে। তবে, PHP স্ক্রিপ্টগুলির মধ্যে দক্ষ টেক্সট প্রক্রিয়াকরণ এবং যাচাইকরণের জন্য PHP-এর বিল্ট-ইন PCRE ফাংশনগুলি বুঝতে এবং ব্যবহার করতে জরুরি।
