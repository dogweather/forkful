---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:15:52.950562-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
টেক্সট খুঁজে বের করা এবং প্রতিস্থাপন করা মানে নির্দিষ্ট স্ট্রিংগুলি খুঁজে বের করে তাদের অন্য কিছুর সাথে পরিবর্তন করা। প্রোগ্রামাররা কোড আপডেট করার জন্য, ত্রুটি সংশোধনের জন্য, অথবা ডেটা পুনঃফরম্যাট করার জন্য এটা করে থাকেন — এটি একটি বিশাল সময় বাঁচানোর কৌশল।

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
