---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:11.328868-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6 \u09B6\u09C7\
  \u09B2, \u09A8\u09BF\u099C\u09C7 \u09A5\u09C7\u0995\u09C7\u0987, CSV \u09AE\u09CD\
  \u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09A8\u09BF\u099C\u09B8\u09CD\u09AC \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09B8\u09AE\u09C3\u09A6\u09CD\u09A7 \u09A8\u09AF\u09BC\u0964 \u09A4\u09AC\u09C7\
  , \u0986\u09AA\u09A8\u09BF `awk`, `sed`, \u098F\u09AC\u0982 `cut` \u098F\u09B0 \u09AE\
  \u09A4\u09CB \u0987\u0989\u09A8\u09BF\u0995\u09CD\u09B8 \u0987\u0989\u099F\u09BF\
  \u09B2\u09BF\u099F\u09BF\u0997\u09C1\u09B2\u09CB \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-04-05T21:53:53.211028-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6 \u09B6\u09C7\u09B2, \u09A8\u09BF\u099C\u09C7 \u09A5\u09C7\
  \u0995\u09C7\u0987, CSV \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\
  \u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u099C\u09B8\u09CD\
  \u09AC \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09AE\u09C3\u09A6\u09CD\u09A7 \u09A8\
  \u09AF\u09BC\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF `awk`, `sed`, \u098F\
  \u09AC\u0982 `cut` \u098F\u09B0 \u09AE\u09A4\u09CB \u0987\u0989\u09A8\u09BF\u0995\
  \u09CD\u09B8 \u0987\u0989\u099F\u09BF\u09B2\u09BF\u099F\u09BF\u0997\u09C1\u09B2\u09CB\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0985\u09A5\u09AC\u09BE\
  \ `csvkit` \u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\u09BF\u09B6\u09C7\u09B7\u09BE\u09DF\
  \u09BF\u09A4 \u099F\u09C1\u09B2\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0986\u09B0\u09CB \u0989\
  \u09A8\u09CD\u09A8\u09A4 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u0964."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
ফিশ শেল, নিজে থেকেই, CSV ম্যানিপুলেশনের জন্য নিজস্ব ফাংশন সমৃদ্ধ নয়। তবে, আপনি `awk`, `sed`, এবং `cut` এর মতো ইউনিক্স ইউটিলিটিগুলো ব্যবহার করে মৌলিক অপারেশনগুলি করতে পারেন অথবা `csvkit` এর মতো বিশেষায়িত টুলস ব্যবহার করতে পারেন আরো উন্নত কাজের জন্য।

### একটি CSV ফাইল পড়া এবং প্রথম কলাম প্রিন্ট করা:
প্রথম কলাম নির্বাচনের জন্য `cut` ব্যবহার করা:
```fish
cut -d ',' -f1 data.csv
```
নমুনা আউটপুট:
```
নাম
অ্যালিস
বব
```

### কলামের মানের উপর ভিত্তি করে CSV সারি ফিল্টার করা:
দ্বিতীয় কলামে "42" ম্যাচ করা সারিগুলি খুঁজতে `awk` ব্যবহার করা:
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
নমুনা আউটপুট:
```
বব,42,লন্ডন
```

### একটি CSV ফাইল মডিফাই করা (যেমন, একটি কলাম যোগ করা):
একটি স্থায়ী মান "NewColumn" সহ একটি কলাম যোগ করতে `awk` ব্যবহার করা:
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
`modified.csv`-এ নমুনা আউটপুট:
```
নাম,বয়স,শহর,নিউকলাম
অ্যালিস,30,নিউ ইয়র্ক,নিউকলাম
বব,42,লন্ডন,নিউকলাম
```

### আরো উন্নত অপারেশনের জন্য `csvkit` ব্যবহার করা:
প্রথমে, নিশ্চিত করুন আপনি `csvkit` ইনস্টল করেছেন। যদি না থাকে, তাহলে pip ব্যবহার করে ইনস্টল করুন: `pip install csvkit`.

**CSV ফাইলকে JSON-এ রূপান্তর করা:**
```fish
csvjson data.csv > data.json
```
`data.json`-এ নমুনা আউটপুট:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**`csvkit`-এর `csvgrep` ব্যবহারের সাথে ফিল্টারিং:**
```fish
csvgrep -c 2 -m 42 data.csv
```
এই কমান্ডটি `csvkit` ব্যবহার করে ফিল্টারিং টাস্ক পুনরাবৃত্তি করে, মান "42" এর জন্য দ্বিতীয় কলামে টার্গেট করে।

সংক্ষেপে, যখন ফিশ শেল নিজেই সরাসরি CSV ম্যানিপুলেশনের সুবিধা দিতে নাও পারে, তার একত্রিত ইউনিক্স ইউটিলিটিগুলির সাথে সংযোগ এবং `csvkit` এর মতো টুলসের উপলব্ধতা CSV ফাইল নিয়ে কাজ করার জন্য শক্তিশালী বিকল্প প্রস্তাব করে।
