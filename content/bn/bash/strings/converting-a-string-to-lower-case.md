---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:43.300434-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09CB\u09AF\u09BC\u09BE\
  \u09B0\u0995\u09C7\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09B8\
  \u0995\u09B2 \u09AC\u09B0\u09CD\u09A3\u09AE\u09BE\u09B2\u09BE \u0985\u0995\u09CD\
  \u09B7\u09B0\u0995\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8 \u09AB\u09B0\u09CD\u09AE\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09AE\u09BE\u09A8\u09A4\u09BE\
  , \u0995\u09C7\u09B8 \u09B8\u09C7\u09A8\u09B8\u09BF\u099F\u09BF\u09AD\u09BF\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:44.210326-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09CB\u09AF\u09BC\u09BE\
  \u09B0\u0995\u09C7\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09B8\
  \u0995\u09B2 \u09AC\u09B0\u09CD\u09A3\u09AE\u09BE\u09B2\u09BE \u0985\u0995\u09CD\
  \u09B7\u09B0\u0995\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8 \u09AB\u09B0\u09CD\u09AE\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09AE\u09BE\u09A8\u09A4\u09BE\
  , \u0995\u09C7\u09B8 \u09B8\u09C7\u09A8\u09B8\u09BF\u099F\u09BF\u09AD\u09BF\u099F\
  \u09BF \u099B\u09BE\u09A1\u09BC\u09BE \u09A4\u09C1\u09B2\u09A8\u09BE, \u098F\u09AC\
  \u0982 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u09AC\u09BE \u0985\u09CD\u09AF\
  \u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8 \u09AE\u09C7\u099F\u09BE\u09A8\u09CB\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7\
  \ \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
এখানে বাশে একটি স্ট্রিংকে লোয়ার কেসে রূপান্তর করার সহজ উপায় দেওয়া হল:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

আউটপুট:

```
make me lower case
```

বাশ 4.0 এবং তার উপরের ভার্সনে প্যারামিটার এক্সপানশন দিয়ে একটি বিল্ট-ইন উপায় আছে:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

আউটপুট:

```
make me lower case
```

## গভীর ডাইভ
বাশ 4.0 এর আগে, স্ট্রিংগুলিকে লোয়ার কেসে রূপান্তর করার জন্য সাধারণত `tr`, `awk`, বা `sed` এর মত বাহ্যিক ইউটিলিটিগুলি ব্যবহার করা হত। এগুলি প্রত্যেকই কেবল কেস পরিবর্তন করার চেয়ে বেশি কিছু উপায়ে স্ট্রিংগুলিকে ম্যানিপুলেট করার বিভিন্ন উপায় প্রদান করে, তবে এগুলি নতুন প্রক্রিয়া তৈরি করতে পারে, পারফরম্যান্স প্রভাবিত করতে পারে।

`${parameter,,pattern}` সিনট্যাক্সের পরিচয় বাশ 4.0 এ একটি দেশীয় ফিচার প্রদান করে যা দ্রুত এবং বাহ্যিক ইউটিলিটির উপর নির্ভর করে না। বাশের মধ্যে বিকল্প উপায়গুলি আছে:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - যেমন উপরে দেখানো হয়েছে।

বাস্তবায়নের দিক থেকে, `${parameter,,pattern}` কেবল এএসসিআই অক্ষরগুলিকেই রূপান্তর করে না; এগুলি ইউটিএফ-৮ সচেতন এবং ইংরেজি ছাড়া ভাষার অক্ষরগুলিকে সামলাতে পারে, যা এগুলিকে আন্তর্জাতিক অ্যাপ্লিকেশনের জন্য বহুমুখী করে তোলে।

## দেখুন এছাড়াও
- বাশ প্যারামিটার এক্সপানশন: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- `tr` কমান্ড: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- এডব্লিউকে প্রোগ্রামিং: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` স্ট্রিম এডিটর: https://www.gnu.org/software/sed/manual/sed.html
