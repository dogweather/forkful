---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:09.412098-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u0999\u09CD\u0995\u09CD\
  \u09AF\u09BE\u099F\u09C7\u09A8\u09C7\u099F \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u09B6\u09AC\u09CD\u09A6\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u098F\u0995\
  \u09A4\u09CD\u09B0\u09C7 \u09AC\u09BE\u0981\u09A7\u09BE\u0987 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BF \u0995\u09C7 \u09AD\u09BE\u09AC\u09C1\u09A8 \u09AF\u09C7\
  \u09A8 \u098F\u0995\u099F\u09BF \u099F\u09CD\u09B0\u09C7\u09A8 \u09AC\u09BE\u09A8\
  \u09BE\u09A8\u09CB \u09B9\u099A\u09CD\u099B\u09C7, \u09B6\u09C1\u09A7\u09C1 \u09AF\
  \u09BE\u09A8\u09AC\u09BE\u09B9\u09A8\u09C7\u09B0 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\
  \u09A4\u09C7 \u09B6\u09AC\u09CD\u09A6 \u09A6\u09BF\u09AF\u09BC\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.120846-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u0999\u09CD\u0995\u09CD\
  \u09AF\u09BE\u099F\u09C7\u09A8\u09C7\u099F \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u09B6\u09AC\u09CD\u09A6\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u098F\u0995\
  \u09A4\u09CD\u09B0\u09C7 \u09AC\u09BE\u0981\u09A7\u09BE\u0987 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BF \u0995\u09C7 \u09AD\u09BE\u09AC\u09C1\u09A8 \u09AF\u09C7\
  \u09A8 \u098F\u0995\u099F\u09BF \u099F\u09CD\u09B0\u09C7\u09A8 \u09AC\u09BE\u09A8\
  \u09BE\u09A8\u09CB \u09B9\u099A\u09CD\u099B\u09C7, \u09B6\u09C1\u09A7\u09C1 \u09AF\
  \u09BE\u09A8\u09AC\u09BE\u09B9\u09A8\u09C7\u09B0 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\
  \u09A4\u09C7 \u09B6\u09AC\u09CD\u09A6 \u09A6\u09BF\u09AF\u09BC\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কি এবং কেন?

স্ট্রিং কঙ্ক্যাটেনেট করা মূলত শব্দগুলিকে একত্রে বাঁধাই করা। এটি কে ভাবুন যেন একটি ট্রেন বানানো হচ্ছে, শুধু যানবাহনের পরিবর্তে শব্দ দিয়ে। প্রোগ্রামাররা এটি করে টেক্সট যেমন নামের সাথে অভিবাদন, অথবা বার্তা এবং ডাটা গঠন করতে, যা নমনীয় হতে হবে।

## কিভাবে:

PHP-তে, কঙ্ক্যাটেনেশন মানে হলো ডট (`.`) দিয়ে কাজ করা। দুটি স্ট্রিং নিন, তাদের মাঝে একটি ডট রাখুন এবং দেখুন! তারা এখন একই।

```PHP
$greeting = 'Hello, ';
$name = 'Alice!';
$message = $greeting . $name;
echo $message;
// আউটপুট: Hello, Alice!
```

সহজ, তাই না? একটি স্পেস যোগ করতে চান? শুধু একটি স্ট্রিং-এ তা যোগ করুন এবং কঙ্ক্যাটেনেট করুন:

```PHP
$firstWord = 'Hello';
$space = ' ';
$secondWord = 'World!';
$sentence = $firstWord . $space . $secondWord;
echo $sentence;
// আউটপুট: Hello World!
```

এবং PHP পেশাদারদের জন্য, আমরা তাদেরকে একসাথে চেইন করতে পারি অথবা শর্টহ্যান্ড (`.= `) ব্যবহার করতে পারি:

```PHP
$message = 'This';
$message .= ' is';
$message .= ' a';
$message .= ' sentence.';
echo $message;
// আউটপুট: This is a sentence.
```

## গভীর ডাইভ

অতীতের দিনগুলিতে, PHP-এর লোকেরা শব্দগুলি একত্রে জোড়া দিতে ডট ব্যবহার করতে হতো। এটি শব্দের জন্য ডাক্ট টেপের মতো। কঙ্ক্যাটেনেশন অপরিহার্য কারণ ডাটা সবসময় আমাদের প্রয়োজন মত ফরম্যাটে দেওয়া হয় না।

বিকল্প প্রসঙ্গে, কয়েকটি আছে। `sprintf()` এবং `printf()` ফাংশনগুলো ফর্ম্যাটেড স্ট্রিংস-কে অনুমোদন করে। ধরুন আপনি একটি মুভি স্ক্রিপ্ট তৈরি করছেন যাতে প্লেসহোল্ডার রয়েছে, এবং এই ফাংশনগুলো অভিনেতার নামগুলি পূরণ করে।

```PHP
$format = 'There are %d monkeys in the %s';
echo sprintf($format, 5, 'tree');
// আউটপুট: There are 5 monkeys in the tree
```

কিন্তু আমাদের বিশ্বস্ত বন্ধু, `implode()` ফাংশনকে ভুলতে পারি না। এটি একটি মেশিনের মতো যা একটি অ্যারে অফ স্ট্রিংস এবং একটি গ্লু স্ট্রিং নিয়ে তাদেরকে একত্রে আটকায়।

```PHP
$array = ['Once', 'upon', 'a', 'time'];
echo implode(' ', $array);
// আউটপুট: Once upon a time
```

আরেকটি ভাবার বিষয় হচ্ছে দক্ষতা। দীর্ঘ স্ট্রিংগুলি বা ভারি অপারেশনের জন্য, `.` ব্যবহার করে তুলনামূলকভাবে ধীর হতে পারে অন্যান্য পদ্ধতির তুলনায় যেমন `implode()` বা আউটপুট বাফারিং করা। কিন্তু প্রায় সব দৈনন্দিন কাজের জন্য, ডট ব্যবহার করে কঙ্ক্যাটেনেশন মনোরম ভাবে কাজ করে।

## আরও দেখুন

আরও জানতে আগ্রহীরা আলোচনায়:

- স্ট্রিং অপারেটরসের উপর অফিসিয়াল PHP ডকুমেন্টেশন আপনার টুলসগুলি জানার জন্য একটি দারুণ স্থান: [PHP String Operators](https://www.php.net/manual/en/language.operators.string.php)
- আরও উন্নত স্ট্রিং ফর্ম্যাটিং জানতে, `sprintf()` এবং `printf()` ফাংশনগুলো দেখুন: [PHP sprintf()](https://www.php.net/manual/en/function.sprintf.php)
- যদি আপনি একটি অ্যারের উপাদানগুলি একত্রিত করতে চান, তাহলে `implode()` সম্পর্কে পড়ুন: [PHP implode()](https://www.php.net/manual/en/function.implode.php)
- দক্ষতা বিষয়ক সচেতনদের জন্য, কঙ্ক্যাটেনেশন বনাম অন্যান্য পদ্ধতির উপর এই আলোচনাটি খুবই জ্ঞানপ্রদ: [Stack Overflow: Efficient String Concatenation](https://stackoverflow.com/questions/3349753/efficient-string-concatenation-in-php)
