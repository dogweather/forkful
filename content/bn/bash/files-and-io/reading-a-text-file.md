---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:22.642113-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09B0\u09AF\u09BC\u09C7\u099B\u09C7 \u09AB\u09BE\u0987\u09B2\u0995\u09C7 \u09B2\
  \u09BE\u0987\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\u09AF\u09BC\u09C0 \u09AA\u09A1\
  \u09BC\u09BE\u09B0 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u09B8\u09B9\u099C\
  \ \u0989\u09AA\u09BE\u09AF\u09BC."
lastmod: '2024-03-17T18:47:44.245372-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7 \u09AB\
  \u09BE\u0987\u09B2\u0995\u09C7 \u09B2\u09BE\u0987\u09A8 \u0985\u09A8\u09C1\u09AF\
  \u09BE\u09AF\u09BC\u09C0 \u09AA\u09A1\u09BC\u09BE\u09B0 \u09B8\u09AC\u099A\u09C7\
  \u09AF\u09BC\u09C7 \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

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
