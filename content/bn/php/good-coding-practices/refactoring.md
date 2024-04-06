---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:43.453231-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8, \u098F\
  \u0995\u099F\u09BF \u0995\u09CD\u09B2\u09BE\u09B8\u09BF\u0995 PHP \u09B8\u09CD\u09A8\
  \u09BF\u09AA\u09C7\u099F \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BF\u099B\u09C1\
  \ \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982 \u09AE\
  \u09CD\u09AF\u09BE\u099C\u09BF\u0995 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u0997\
  \ \u0995\u09B0\u09BF\u0964 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u09B0\u09BF\u0982 \u0995\u09B0\u09BE\u09B0 \u0986\u0997\u09C7, \u0986\u09AE\u09BE\
  \u09A6\u09C7\u09B0 \u0995\u09CB\u09A1 \u098F\u09AE\u09A8 \u09A6\u09C7\u0996\u09BE\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7."
lastmod: '2024-04-05T21:53:52.560986-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8, \u098F\u0995\u099F\u09BF \u0995\u09CD\u09B2\u09BE\
  \u09B8\u09BF\u0995 PHP \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BF\u099B\u09C1 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09B0\u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\u099C\u09BF\u0995 \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u0997 \u0995\u09B0\u09BF\u0964 \u09B0\u09BF\u09AB\
  \u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982 \u0995\u09B0\u09BE\u09B0\
  \ \u0986\u0997\u09C7, \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u0995\u09CB\u09A1 \u098F\
  \u09AE\u09A8 \u09A6\u09C7\u0996\u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:
চলুন, একটি ক্লাসিক PHP স্নিপেট নিয়ে কিছু রিফ্যাক্টরিং ম্যাজিক প্রয়োগ করি।

রিফ্যাক্টরিং করার আগে, আমাদের কোড এমন দেখাতে পারে:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

কিন্তু আমরা এই কোডটি রিফ্যাক্টর করে এর স্পষ্টতা এবং মডুলারিটি উন্নতি করতে পারি:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
`printOrderDetails` ফাংশনটি ছোট ছোট ফাংশনগুলিতে ভাঙ্গার মাধ্যমে, আমাদের কোড আরও পড়া সহজ এবং ডিবাগ করা সহজ হয়।

## গভীর ডুব
রিফ্যাক্টরিং তার মূল পায় আদি 1990 এর দশকের স্মালটক প্রোগ্রামিং কমিউনিটিতে এবং মার্টিন ফাওলারের ঐতিহাসিক বই "Refactoring: Improving the Design of Existing Code" (১৯৯৯) দ্বারা আরও জনপ্রিয় হয়ে ওঠে। যদিও রিফ্যাক্টরিং যেকোনো প্রোগ্রামিং ভাষায় প্রযোজ্য, PHP-র গতিশীল প্রকৃতি কিছু অনন্য চ্যালেঞ্জ এবং সুযোগের সৃষ্টি করে।

রিফ্যাক্টরিং এর বিকল্প হতে পারে কোড শুরু থেকে নতুন করে লেখা, যা প্রায়ই বেশি ঝুঁকিপূর্ণ এবং সময় সাপেক্ষ। PHP ইকোসিস্টেমে, PHPStan এবং Rector মত টুলগুলি যথাক্রমে কিছু রিফ্যাক্টরিং অপারেশন স্বয়ংক্রিয়ভাবে চিহ্নিত করে এবং সম্পাদনা করতে পারে। বাস্তবায়নের দিক থেকে, রিফ্যাক্টরিংগুলি ছোট রাখা এবং ইউনিট টেস্ট দিয়ে ব্যাপকভাবে পরীক্ষা করা বাগযুক্ত হওয়া ছাড়াই সফল রিফ্যাক্টরিং নিশ্চিত করার মূল অনুশীলন।

## আরও দেখুন
- মার্টিন ফাওলারের রিফ্যাক্টরিং বই: https://martinfowler.com/books/refactoring.html
- PHPStan, একটি PHP স্ট্যাটিক এনালাইসিস টূল: https://phpstan.org/
- Rector, PHP কোডের স্বয়ংক্রিয় রিফ্যাক্টরিং-এর জন্য একটি টূল: https://getrector.org/
- PHP ইউনিট টেস্টিং পিএইউপিইউটেস্ট দিয়ে: https://phpunit.de/
