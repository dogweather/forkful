---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:22.642113-06:00
description: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u0995\u09A8\u09CD\u099F\u09C7\
  \u09A8\u09CD\u099F \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09C7 \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09A1\
  \u09C7\u099F\u09BE, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7, \u0985\
  \u09A5\u09AC\u09BE \u09B8\u09C7\u0987 \u099F\u09C7\u0995\u09CD\u09B8\u099F-\u09AB\
  \u09BE\u0987\u09B2 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:44.245372-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AB\
  \u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\
  \u09CD\u099F \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u09C7 \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09A1\u09C7\
  \u099F\u09BE, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\
  \u09AC\u09BE \u09B8\u09C7\u0987 \u099F\u09C7\u0995\u09CD\u09B8\u099F-\u09AB\u09BE\
  \u0987\u09B2 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AD\u09BF\u09A4\
  \u09CD\u09A4\u09BF\u0995 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u0985\u099F\
  \u09CB\u09AE\u09C7\u099F \u0995\u09B0\u09A4\u09C7 \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u09A8\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কি এবং কেন?
একটি টেক্সট ফাইল পড়া মানে হল ফাইল থেকে কন্টেন্ট আপনার স্ক্রিপ্টে আনা। প্রোগ্রামাররা এটি ডেটা, কনফিগারেশন নিয়ে কাজ করতে, অথবা সেই টেক্সট-ফাইল কন্টেন্ট ভিত্তিক সিস্টেম অটোমেট করতে করে থাকেন।

## কিভাবে:
এখানে রয়েছে ফাইলকে লাইন অনুযায়ী পড়ার সবচেয়ে সহজ উপায়:

```Bash
while IFS= read -r line; do
    echo "টেক্সট: $line"
done < "yourfile.txt"
```

সম্পূর্ণ ফাইল একবারে চাই? এটি চেষ্টা করুন:

```Bash
file_content=$(<yourfile.txt)
echo "$file_content"
```

নাকি আপনার একটি নির্দিষ্ট লাইন প্রয়োজন, ধরুন লাইন ৪?

```Bash
sed '4q;d' yourfile.txt
```

লাইন ৪ পড়ার জন্য নমুনা আউটপুট:

```
This is the content of line four.
```

## গভীর ডুব
আগের দিনে, আমাদের কাছে ফ্যান্সি IDEs ছিল না, ছিল টার্মিনাল এবং সাধারণ টেক্সট এডিটর। UNIX টুলগুলি এমন একটি দর্শনের সাথে ডিজাইন করা হয়েছিল যে, এগুলি ভালোভাবে একটি কাজ করে। `cat`, `less`, `sed`, এবং `awk` হল টেক্সট ম্যানিপুলেট করার বিশেষজ্ঞ।

বাশে একটি ফাইল পড়া এই টুলগুলি ব্যবহার করে, সাথে বাশ নিজস্ব রিডাইরেক্টস এবং লুপগুলি। যেমন, `while` এর সাথে `read` বড় ফাইলের জন্য মেমোরি দক্ষতার জন্য ভালো, আপনি লাইন অনুযায়ী পড়ছেন, সবকিছু মেমোরিতে ডাম্প না করে।

`sed` হল একটি স্ট্রীম এডিটর। `sed '4q;d' yourfile.txt` ব্যবহার করে একটি নির্দিষ্ট লাইন আহরণ করা বলে `sed`-কে চতুর্থ লাইনের পরে ছেড়ে দিতে (`4q`) এবং তারপর সেই লাইনটি প্রিন্ট করতে (`;d`)।

বিকল্প পদ্ধতি আছে। `awk` টেক্সট প্রসেসিং-এ ক্ষমতাবান। টেক্সট প্রসেসিং জটিল হলে বাশের মধ্যে পার্ল এবং পাইথন স্ক্রিপ্ট আহ্‌বান করা যেতে পারে। এই সব টুল ও ভাষাগুলির প্রত্যেকের নিজস্ব ব্যবহারের ক্ষেত্র এবং পারফরম্যান্স বিবেচনা রয়েছে।

## দেখুনও
1. বাশ স্ক্রিপ্টিং গাইড: https://www.gnu.org/software/bash/manual/
2. `sed` এবং `awk` ১০১ হ্যাকস: https://www.thegeekstuff.com/2009/12/unix-sed-tutorial-6-examples-to-edit-file-in-place/
3. লিনাক্স কমান্ড লাইন টেক্সট প্রসেসিং `grep`, `awk`, `sed`, `sort`, এবং বন্ধুরা সহ: https://github.com/learnbyexample/Command-line-text-processing
