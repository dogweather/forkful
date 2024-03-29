---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:52.378931-06:00
description: "CLI (\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09AB\u09C7\u09B8) \u098F\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u098F\u0995-\u09B2\u09BE\u0987\u09A8\u09C7\u09B0 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\u09AE\u09CD\u09AA\u09BE\
  \u09A6\u09A8\u09BE \u0995\u09B0\u09BE \u09AF\u09BE\u09DF, \u09AF\u09C7\u09AE\u09A8\
  \ - \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u09AA\
  \u09DC\u09BE, \u0986\u09AA\u09A1\u09C7\u099F \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\
  \u09BE \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u0987\u09A4\u09CD\u09AF\
  \u09BE\u09A6\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.220526-06:00'
model: gpt-4-0125-preview
summary: "CLI (\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09AB\u09C7\u09B8) \u098F\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u098F\u0995-\u09B2\u09BE\u0987\u09A8\u09C7\u09B0 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\u09AE\u09CD\u09AA\u09BE\
  \u09A6\u09A8\u09BE \u0995\u09B0\u09BE \u09AF\u09BE\u09DF, \u09AF\u09C7\u09AE\u09A8\
  \ - \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u09AA\
  \u09DC\u09BE, \u0986\u09AA\u09A1\u09C7\u099F \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\
  \u09BE \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u0987\u09A4\u09CD\u09AF\
  \u09BE\u09A6\u09BF\u2026"
title: "CLI \u0993\u09DF\u09BE\u09A8-\u09B2\u09BE\u0987\u09A8\u09BE\u09B0\u09CD\u09B8\
  \ \u09A6\u09BF\u09DF\u09C7 \u09AB\u09BE\u0987\u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

CLI (কমান্ড লাইন ইন্টারফেস) এর মাধ্যমে এক-লাইনের স্ক্রিপ্ট ব্যবহার করে ফাইল সম্পাদনা করা যায়, যেমন - ফাইল তৈরি করা, পড়া, আপডেট করা, অথবা মুছে ফেলা ইত্যাদি কাজ, সবকিছুই টার্মিনাল থেকে সম্পন্ন করা সম্ভব। প্রোগ্রামারগণ দক্ষতা, স্বয়ংক্রিয়তা এবং ফাইল অপারেশনগুলি লিনাক্স সার্ভার বা সিস্টেমে হ্যান্ডেল করার অসাধারণ শক্তির জন্য এই প্রযুক্তিটি ব্যবহার করে থাকেন, যেখানে গ্রাফিক্যাল ইন্টারফেস পাওয়া যেতে পারে না।

## কিভাবে:

এখানে কিছু শক্তিশালী এক-লাইনার এবং তারা কি করতে পারে তা দেয়া হল:

১. **একটি ফাইল তৈরি করা এবং তাতে লেখা:**
```Bash
echo "Hello, Linux Journal Readers!" > greetings.txt
```
এটি `greetings.txt` ফাইল তৈরি করে (অথবা যদি আগে থেকে থাকে তবে তা পুনর্লিখন করে) এবং তাতে "Hello, Linux Journal Readers!" ফ্রেজটি যোগ করে।

২. **একটি বিদ্যমান ফাইলে লেখা যোগ করা:**
```Bash
echo "Welcome to Bash programming." >> greetings.txt
```
এটি `greetings.txt` ফাইলের শেষে "Welcome to Bash programming." নামক একটি নতুন লাইন যোগ করে।

৩. **একটি ফাইলের বিষয়বস্তু পড়া:**
```Bash
cat greetings.txt
```
আউটপুট:
```
Hello, Linux Journal Readers!
Welcome to Bash programming.
```

৪. **ফাইলে নির্দিষ্ট একটি লাইন খোঁজা (‘grep’ ব্যবহার করে):**
```Bash
grep "Bash" greetings.txt
```
এটি "Bash" শব্দটি যুক্ত লাইনগুলি খুঁজে বের করে এবং প্রদর্শন করে; এই উদাহরণে, এটি "Welcome to Bash programming." রিটার্ন করে।

৫. **বর্তমান ডিরেক্টরিতে তাদের পরিবর্তনের তারিখ অনুযায়ী সব ফাইল তালিকাভুক্ত করা:**
```Bash
ls -lt
```
সবচেয়ে নতুন থেকে সাজানো ফাইল প্রদর্শন করে।

৬. **বাল্কে `.txt` ফাইলগুলিকে `.md` (Markdown) এ পুনঃনামকরণ:**
```Bash
for file in *.txt; do mv "$file" "${file%.txt}.md"; done
```
এই লুপটি বর্তমান ডিরেক্টরিতে প্রতিটি `.txt` ফাইলের মাধ্যমে যায় এবং তা `.md` তে পুনঃনামকরণ করে।

এই CLI এক-লাইনারগুলি Bash এর শক্তিকে কাজে লাগিয়ে দ্রুত এবং কার্যকরী ফাইল সম্পাদনা কৌশলে পারদর্শী যেকোন প্রোগ্রামারের জন্য অপরিহার্য।

## গভীরে প্রবেশ

Bash শেল, যেটি বেশিরভাগ UNIX-এর জাতীয় সিস্টেমে একটি স্থায়ী উপাদান, বোর্ন শেল (sh) থেকে বিকশিত হয়েছিল, যা 1979 সালে Version 7 Unix এ প্রবর্তিত হয়েছিল। Bash তার পূর্বসূরীর ক্ষমতাবলীকে উন্নত স্ক্রিপ্টিং বৈশিষ্ট্যের মাধ্যমে বিস্তারিত করেছে যা এটিকে সিস্টেম প্রশাসকেরা এবং প্রোগ্রামারদের মাঝে জনপ্রিয় করেছে।

যদিও Bash ফাইল সম্পাদনার জন্য অসাধারণ শক্তিশালী, এর কিছু সীমাবদ্ধতা রয়েছে, যেমন টেক্সট-ভিত্তিক হওয়ায়, জটিল অপারেশনগুলি (যেমন যেগুলি বাইনারি ডাটা জড়িত) অনেক সময় ব্যবহার করা ঝামেলাসঙ্কুল অথবা অকার্যকর হতে পারে যদি তা Python এর মত কোনো প্রোগ্রামিং ভাষা ব্যবহার করা হয় যা এই ধরণের ক্ষমতা কে মাথায় রেখে ডিজাইন করা হয়েছে।

ফাইল সম্পাদনার জন্য Bash স্ক্রিপ্টিং এর বিকল্প হিসেবে Python স্ক্রিপ্‌টিং `os` এবং `shutil` লাইব্রেরিগুলি ব্যবহার করতে পারে, যা আরও পঠনীয় সিনট্যাক্স এবং জটিল পরিস্থিতিগুলিতে সুন্দরভাবে মোকাবিলা করতে পারে। তবে, Bash এর সর্বব্যাপৃত উপস্থিতি এবং অধিকাংশ ফাইল কাজের জন্য এর দক্ষতা এর জনপ্রিয়তাকে নিশ্চিত করে।

এছাড়াও, বুঝতে পারা যে Bash কিভাবে ফাইলের সাথে আচরণ করে (Unix/Linux প্যারাডমে সবকিছুই একটি ফাইল) এবং এর অন্তর্নিহিত কমান্ডগুলি (`awk`, `sed`, `grep` ইত্যাদি) কে কিভাবে দক্ষতার সাথে ব্যবহার করা যায় তা একজন প্রোগ্রামারকে আরও দক্ষ এবং কার্যকর স্ক্রিপ্ট লিখতে সাহায্য করে এবং শেলের ক্ষমতা এবং এর ঐতিহাসিক প্রেক্ষাপটের গভীর জ্ঞান একজন প্রোগ্রামারের ফাইল সম্পাদনা এবং কমান্ড লাইন থেকে সরাসরি বিস্তৃত রেঞ্জের কাজ সম্পাদনের সক্ষমতা বৃদ্ধি করে।
