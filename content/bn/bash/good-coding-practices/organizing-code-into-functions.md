---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:41.725221-06:00
description: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\
  \ \u09AD\u09BE\u0997 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u099B\u09CB\
  \u099F, \u09AA\u09C1\u09A8\u09B0\u09CD\u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u09AF\u09CB\u0997\u09CD\u09AF \u09AC\u09CD\u09B2\u0995\u09C7 \u09AD\u09BE\u0997\
  \ \u0995\u09B0\u09BE \u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u0995\u09BE\u099C \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF \u0995\u09CB\
  \u09A1\u0995\u09C7 \u0986\u09B0\u0993 \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\
  \u09B0, \u09AC\u09CB\u09A7\u0997\u09AE\u09CD\u09AF \u098F\u09AC\u0982 \u09A1\u09BF\
  \u09AC\u09BE\u0997 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.232772-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u09AD\
  \u09BE\u0997 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09CD\u0995\u09CD\
  \u09B0\u09BF\u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u099B\u09CB\u099F\
  , \u09AA\u09C1\u09A8\u09B0\u09CD\u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09AF\
  \u09CB\u0997\u09CD\u09AF \u09AC\u09CD\u09B2\u0995\u09C7 \u09AD\u09BE\u0997 \u0995\
  \u09B0\u09BE \u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF \u0995\u09CB\u09A1\
  \u0995\u09C7 \u0986\u09B0\u0993 \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0\
  , \u09AC\u09CB\u09A7\u0997\u09AE\u09CD\u09AF \u098F\u09AC\u0982 \u09A1\u09BF\u09AC\
  \u09BE\u0997 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7\u2026"
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
কোডকে ফাংশনে ভাগ করা মানে স্ক্রিপ্টগুলিকে ছোট, পুনর্ব্যবহারযোগ্য ব্লকে ভাগ করা যা নির্দিষ্ট কাজ করে। এটি কোডকে আরও পরিষ্কার, বোধগম্য এবং ডিবাগ করা সহজ করে তোলে।

## কিভাবে:
Bash-এ একটি সহজ ফাংশন তৈরি করা:

```Bash
greet() {
  echo "Hello, $1!"
}
```

একটি প্যারামিটার সহ ফাংশন কল করে এটি ব্যবহার করুন:

```Bash
greet "World"  # আউটপুট: Hello, World!
```

ফাংশনগুলি নিউমেরিক স্ট্যাটাস কোডের জন্য `return` ব্যবহার করে মান ফেরত দিতে পারে (আসল ডেটা ফেরতের জন্য নয়):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # আউটপুট: 7
```

মনে রাখবেন `$?` সর্বশেষ কমান্ডের ফেরত মান গ্রহণ করে, যা `add`-এর নিউমেরিক ফলাফল।

## গভীর ডুব
Bash-এ, শুরু থেকেই কোডকে কম্পার্টমেন্টালাইজ করার একটি উপায় ফাংশন। ঐতিহাসিকভাবে, কোডের মান উন্নতির জন্য ১৯৬০ দশকে প্রবর্তিত গঠনগত প্রোগ্রামিং নীতির সাথে ফাংশনের ব্যবহার সামঞ্জস্যপূর্ণ।

ফাংশনের বিকল্প হিসাবে স্ক্রিপ্ট ফাইলগুলি সোর্সিং বা অ্যালিয়াস ব্যবহার রয়েছে, তবে এগুলি একই মাত্রার মডুলারিটি এবং পুনর্ব্যবহারের সুবিধা দেয় না।

Bash-এ একটি লক্ষণীয় বাস্তবায়নের বিস্তারিত হল যে ফাংশনগুলি প্রথম শ্রেণীর নাগরিক; অন্যান্য ভাষার মতো একটি নির্দিষ্ট ঘোষণা কীওয়ার্ড যেমন `function` নেই, যদিও Bash-এ পাঠযোগ্যতার জন্য `function` ঐচ্ছিক। ফাংশনের স্কোপও আকর্ষণীয় - ডিফল্ট হিসেবে ভেরিয়েবলগুলি গ্লোবাল, যা যথাযথভাবে পরিচালনা না করা হলে অনাকাঙ্ক্ষিত আচরণের জন্য পথ প্রশস্ত করে।

## আরও দেখুন
- শেল ফাংশন সম্পর্কিত Bash ম্যানুয়াল: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- উন্নত Bash-স্ক্রিপ্টিং গাইড: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell"-এ ফাংশন স্ক্রিপ্টিং সংক্রান্ত গভীর ধারণা ও প্রথা বিষয়ে।
