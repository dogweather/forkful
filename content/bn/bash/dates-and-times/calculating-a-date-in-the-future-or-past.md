---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:29.542563-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Bash-\u098F, \u0986\u09AA\u09A8\
  \u09BF `date` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 `-d` \u09AB\u09CD\u09B2\u09BE\u0997 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09B0\u09BF\u09AC\u09B0\
  \u09CD\u09A4\u09A3 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\
  \u09CB \u0995\u09BF\u09AD\u09BE\u09AC\u09C7."
lastmod: '2024-03-17T18:47:44.241383-06:00'
model: gpt-4-0125-preview
summary: "Bash-\u098F, \u0986\u09AA\u09A8\u09BF `date` \u0995\u09AE\u09BE\u09A8\u09CD\
  \u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 `-d` \u09AB\u09CD\u09B2\u09BE\u0997\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A3 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\
  \u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
Bash-এ, আপনি `date` কমান্ডের সাথে `-d` ফ্লাগ ব্যবহার করে তারিখ পরিবর্তণ করতে পারেন। এখানে দেখানো হলো কিভাবে:

```Bash
# বর্তমান তারিখ
date

# ভবিষ্যত তারিখ: এখন থেকে 10 দিন পরে
date -d "+10 days"

# অতীতের তারিখ: 10 দিন আগে
date -d "-10 days"

# নির্দিষ্ট ভবিষ্যত তারিখ: সপ্তাহ, মাস, বছর যোগ করা
date -d "+1 month"
date -d "+2 weeks"
date -d "+1 year"

# ভবিষ্যত তারিখের জন্য নমুনা আউটপুট
Mon 31 Jan 2023 12:34:56 PM PST
```

## গভীরে ডুব দেওয়া
স্ক্রিপ্টিং এবং প্রোগ্রামিংয়ে তারিখ পরিবর্তন একটি সাধারণ প্রয়োজন। ঐতিহাসিকভাবে, লিপ বছর, টাইমজোন ইত্যাদি পরিচালনা করা অধিক জটিল এবং ভুলভ্রান্তিপূর্ণ ছিল। Unix-এর মতো সিস্টেমে, `date` কমান্ডে সহজ তারিখ হিসেবের জন্য অপশনগুলি যোগ করা হয়েছে।

বিকল্পগুলি মধ্যে আছে শেল অঙ্ক অথবা `awk` বা `perl` এর মতো বাহ্যিক টুল ব্যবহার করা জটিল তারিখ যুক্তির জন্য, কিন্তু মৌলিক অপারেশনের জন্য `date` কমান্ড সবচেয়ে সহজ এবং সরাসরি থাকে। অভ্যন্তরীণভাবে, `date` কমান্ড সময় গণনার জটিলতা পরিচালনা করতে সিস্টেম লাইব্রেরিগুলি ব্যবহার করে, এটি ব্যবহারকারী থেকে আবষ্ট্রাক্ট করে।

## আরও দেখুন
- GNU Coreutils ম্যানুয়াল তারিখ অধ্যায়: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- আরও উদাহরণ ও ব্যবহার ক্ষেত্র: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- উন্নত Bash-স্ক্রিপ্টিং গাইড: https://tldp.org/LDP/abs/html/abs-guide.html
