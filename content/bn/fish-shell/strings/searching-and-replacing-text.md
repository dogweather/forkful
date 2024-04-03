---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:52.950562-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\
  \ 'cat' \u098F\u09B0 \u09B8\u09AC \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7 'dog' \u0995\u09B0\u09BF\
  \u0964."
lastmod: '2024-03-17T18:47:44.480677-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09AF\u09BC\u09C7 'cat' \u098F\u09B0 \u09B8\u09AC \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\
  \u09B0\u09C7 'dog' \u0995\u09B0\u09BF\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
চলুন একটি স্ট্রিংয়ে 'cat' এর সব উদাহরণ পরিবর্তন করে 'dog' করি।

```Fish Shell
echo "One cat, two cats, three cats." | string replace -a 'cat' 'dog'
```
নমুনা আউটপুট:
```
One dog, two dogs, three dogs.
```
`pets.txt` নামের একটি ফাইলে টেক্সট প্রতিস্থাপন করা:

```Fish Shell
string replace -a 'cat' 'dog' < pets.txt > updated_pets.txt
```

প্যাটার্নের জন্য ভেরিয়েবল ব্যবহার করা:

```Fish Shell
set old "cat"
set new "dog"
string replace -a $old $new < pets.txt > updated_pets.txt
```

## গভীর ডাইভ
টেক্সট এডিটরগুলিতে অনুসন্ধান এবং প্রতিস্থাপনের ব্যাপারটি প্রাথমিককাল থেকেই রয়েছে। Unix-এ স্ট্রিম এডিটিংয়ের জন্য `sed` ভাবুন — সেটি পুরোনো স্কুলের ঠান্ডা বিষয়। Fish এটিকে আরও সহজ করেছে, `string` কমান্ডের মাধ্যমে। সাধারণ ক্ষেত্রের জন্য কম ভুল এবং আরও কম ঝামেলা সহ এটি আরও মার্জিত। বিকল্প? নিশ্চিত: `sed`, `awk`, পার্ল স্ক্রিপ্টস, এমনকি `vim` ম্যাক্রোস। কিন্তু Fish-এর `string` কমান্ড হল সাধারণ ক্ষেত্রের জন্য কম ভুল সম্পর্কিত এবং আরও মার্জিত।

## আরও দেখুন:
- Fish Shell-এর `string` কমান্ডের উপর অফিসিয়াল ডকুমেন্টেশন: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed by Example, Part 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- AWK ল্যাঙ্গুয়েজ প্রোগ্রামিং — স্ট্রিং ফাংশনস: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
