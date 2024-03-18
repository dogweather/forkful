---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:21.532709-06:00
description: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2 \u09B0\u09C7\u0996\u09C7 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09BE \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\
  \ \u09A5\u09C7\u0995\u09C7 \u0985\u09AF\u09BE\u099A\u09BF\u09A4 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AC\u09BE \u09B8\u09BF\u0995\u09CB\u09AF\u09BC\u09C7\u09A8\u09CD\
  \u09B8 \u09AC\u09BE\u09A6 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC\
  \u09BE, \u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\u09AE\u2026"
lastmod: '2024-03-17T18:47:44.479700-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2 \u09B0\u09C7\u0996\u09C7 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09BE \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\
  \ \u09A5\u09C7\u0995\u09C7 \u0985\u09AF\u09BE\u099A\u09BF\u09A4 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AC\u09BE \u09B8\u09BF\u0995\u09CB\u09AF\u09BC\u09C7\u09A8\u09CD\
  \u09B8 \u09AC\u09BE\u09A6 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC\
  \u09BE, \u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\u09AE\u2026"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?

একটি নির্দিষ্ট প্যাটার্নের সাথে মিল রেখে অক্ষর মুছে ফেলা মূলত স্ট্রিং বা ফাইলের বিষয়বস্তু থেকে অযাচিত অক্ষর বা সিকোয়েন্স বাদ দিয়ে দেয়া, যা নিয়ম অনুসারে প্যাটার্ন হিসেবে পরিচিত। প্রোগ্রামাররা ডাটা পরিষ্কার করতে, প্রক্রিয়াজন্য তৈরি করতে বা অর্থপূর্ণ তথ্য বের করতে এটি করে থাকেন।

## কীভাবে:

```Fish Shell
# স্ট্রিং থেকে সংখ্যা মুছে ফেলা
set string "Fish123Shell"
echo $string | string replace -ra '[0-9]' ''
# আউটপুট: FishShell

# ছোট হাতের অক্ষর ছাড়া সবকিছু বাদ দেওয়া
set noisy_string "F!i@s#h$%S^h&e*l(l)__+"
echo $noisy_string | string match -r '[a-z]+'
# আউটপুট: ishhell
```

## গভীর ডাইভ

Fish Shell-এ, জাদু ঘটে `string` ইউটিলিটির মাধ্যমে, যা একটি দক্ষ অন্তর্নির্মিত স্ট্রিং অপারেশন টূল - যা সংস্করণ 2.3.0-এ প্রবর্তিত। এর আগে, ব্যবহারকারীরা `sed` বা `awk` এর মত UNIX অপারেশনগুলোর উপর নির্ভর করত। পরিবর্তনের কারণ কী? সহজতা এবং একীকরণ। অভ্যন্তরীণ সমাধানের মাধ্যমে স্ট্রিং ম্যানিপুলেশন সহজ হয়, যা স্ক্রিপ্টগুলি আরও পাঠনীয় ও রক্ষণাবেক্ষণযোগ্য করে তোলে।

বিকল্প? অবশ্যই, প্রাচীন `sed` এখনো কাজ করতে পারে:

```Fish Shell
set old_school_string "Fish@Shell2023"
echo $old_school_string | sed 's/[0-9]//g'
# আউটপুট: Fish@Shell
```

কিন্তু Fish নিজের টুলগুলো ব্যবহার না করার কি কারণ? বাস্তবায়নে, `string replace` এর একটি `-r` অপশন আছে যা রেগেক্স প্যাটার্নগুলি সক্ষম করে। `-a` কমান্ডটি সব ম্যাচের উপর প্রযোজ্য, এবং শেষে একটি '' যোগ করে এটি কিছুর সাথে প্রতিস্থাপনের পরিবর্তে মুছে দেওয়ার জন্য বলে, অর্থাৎ মুছে ফেলে। `string match` ব্যবহার করুন যখন রাখার জন্য একটি প্যাটার্ন খুঁজছেন, যা বাদ দেওয়ার বিপরীত।

## আরও দেখুন

- অফিশিয়াল Fish Shell ডকুমেন্টেশন `string` নিয়ে: https://fishshell.com/docs/current/cmds/string.html
- প্যাটার্নগুলিতে গভীর ডাইভের জন্য Regex টিউটোরিয়াল: https://www.regular-expressions.info/
- Sed & Awk, প্রাচীন টেক্সট পাওয়ারগুলি: একটি পরিচিতি: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html
