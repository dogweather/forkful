---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:41.774240-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0982\u09B6 \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\u09BE \u2014 \u09AD\u09BE\u09AC\u09C1\
  \u09A8 \u098F\u09AE\u09A8 \u09AF\u09C7\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09CB\
  \u09AF\u09BC\u09C7\u099F\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09B8\u09C1\u09A4\
  \u09BE\u09B0 \u098F\u0995 \u099F\u09C1\u0995\u09B0\u09CB \u0995\u09C7\u099F\u09C7\
  \ \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u099A\u09CD\u099B\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.212359-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0982\u09B6 \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\u09BE \u2014 \u09AD\u09BE\u09AC\u09C1\
  \u09A8 \u098F\u09AE\u09A8 \u09AF\u09C7\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09CB\
  \u09AF\u09BC\u09C7\u099F\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09B8\u09C1\u09A4\
  \u09BE\u09B0 \u098F\u0995 \u099F\u09C1\u0995\u09B0\u09CB \u0995\u09C7\u099F\u09C7\
  \ \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u099A\u09CD\u099B\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C7 \u09AE\u09BF\
  \u09B6\u09CD\u09B0\u09BF\u09A4 \u09A4\u09A5\u09CD\u09AF \u0986\u09B2\u09BE\u09A6\
  \u09BE \u0995\u09B0\u09BE, \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\
  \u09B0\u09BE \u09AC\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\
  \u09C7\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
এখানে Bash এর মধ্যে সাবস্ট্রিং এক্সট্র্যাকশন সম্পর্কে সংক্ষিপ্ত ধারণা দেওয়া হল:

```Bash
# ব্যবহার করা হচ্ছে ${string:start:length}
text="The quick brown fox"
substring=${text:4:5}
echo $substring  # আউটপুট 'quick'

# ডিফল্ট লেংথ হল স্ট্রিংটির বাকি অংশ
substring=${text:16}
echo $substring  # আউটপুট 'fox'

# নেগেটিভ স্টার্ট ইনডেক্স (স্ট্রিংটির শেষ থেকে)
substring=${text: -3}
echo $substring  # আউটপুট 'fox'
```

## গভীরে ডুব
Bash দীর্ঘদিন ধরে স্ট্রিং নিয়ে কাজ করে আসছে। সাবস্ট্রিং এক্সট্র্যাকশন একটি পুরানো কৌশল, কিন্তু এখনও খুবই উপযোগী। অত্যাধুনিক টূলস না থাকার আগে, আমাদের কাছে ছিল প্যারামিটার এক্সপেনশন – `${}` সিনট্যাক্স – এবং এটি সময়ের পরীক্ষায় টিকে আছে।

বিকল্প? অবশ্যই। `awk`, `cut`, এবং `grep` সবাই নিজেদের মতো করে স্ট্রিং কেটে ও বানাতে পারে। কিন্তু একটি দ্রুত, কোনো অতিরিক্ত প্রক্রিয়া ছাড়াই কাজ করার জন্য, Bash এর অন্তর্ভুক্ত পদ্ধতি দক্ষ।

বাস্তবায়নের দিক থেকে, Bash সমস্যা ছাড়াই সাবস্ট্রিং বের করে আনে। এটি আপনার স্ট্রিংয়ের ভেতর কি আছে তা নিয়ে চিন্তিত নয়: টেক্সট, সংখ্যা, ইউনিকর্ন ইমোজি – যাই হোক না কেন। শুধুমাত্র শুরু এবং শেষ নির্দেশ করুন, এবং এটি সেই অংশটি অন্ধভাবে কেটে নেবে।

## আরো দেখুন
আরো গভীরে ডুব দিতে এই লিঙ্কগুলি দেখুন:

- প্যারামিটার এক্সপেনশন সম্পর্কিত Bash এর ম্যানুয়াল: `man bash` এবং *Parameter Expansion* এর জন্য খুঁজুন
- `awk` এবং `grep` এর গভীর বিশ্লেষণ: [Awk Tutorial](https://www.gnu.org/software/gawk/manual/) এবং [Grep Manual](https://www.gnu.org/software/grep/manual/grep.html)
- Bash এর মাধ্যমে স্ট্রিং ম্যানিপুলেশনের আরও ব্যাপক দৃষ্টিভঙ্গি: [Bash String Manipulation Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
