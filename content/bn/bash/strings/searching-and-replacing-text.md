---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:37.543560-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8\u09C7\u09B0 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u0995\u09C0\
  \u09AD\u09BE\u09AC\u09C7 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u0997 \u0995\u09B0\
  \u09AC\u09C7\u09A8 \u09A4\u09BE \u09B0\u09AF\u09BC\u09C7\u099B\u09C7: 1. `sed` \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8."
lastmod: '2024-03-17T18:47:44.208132-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u0985\
  \u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\
  \u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u0995\u09CD\u09B7\
  \u09AE\u09A4\u09BE \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u09AA\u09CD\u09B0\u09AF\
  \u09BC\u09CB\u0997 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u09B0\u09AF\u09BC\
  \u09C7\u099B\u09C7."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
এখানে ব্যাশে অনুসন্ধান এবং প্রতিস্থাপনের ক্ষমতা কীভাবে প্রয়োগ করবেন তা রয়েছে:

1. `sed` ব্যবহার করে স্ট্রিংয়ের মধ্যে টেক্সট প্রতিস্থাপন:
```Bash
echo "Hello world" | sed 's/world/universe/'
# আউটপুট: Hello universe
```

2. ফাইলে টেক্সট প্রতিস্থাপন করে পরিবর্তনগুলি সংরক্ষণ:
```Bash
sed -i 's/old_text/new_text/g' file.txt
```

3. আপনার অনুসন্ধান এবং প্রতিস্থাপনে ভেরিয়েবল ব্যবহার করুন:
```Bash
old="apple"
new="banana"
sed "s/$old/$new/g" <<< "I like apple pies"
# আউটপুট: I like banana pies
```

মনে রাখবেন, শেষের `g` মানে "গ্লোবাল", সুতরাং আপনি লাইনের প্রথম ম্যাচটি নয়, প্রতিটি ম্যাচ পরিবর্তন করেন।

## গভীর ডাইভ
ইউনিক্স-সদৃশ সিস্টেমগুলিতে টেক্সট প্রক্রিয়াজাতকরণের জন্য বহুকাল ধরে টুলস রয়েছে। `sed`, যা স্ট্রিম এডিটরের জন্য সংক্ষিপ্ত, এমন একটি টুল এবং এটি ১৯৭০-এর দশক থেকে বিদ্যমান। এটি শুধু সরল প্রতিস্থাপনের জন্য নয়; `sed` জটিল প্যাটার্নেও টেক্সট কাটা এবং সাজাতে পারে।

বিকল্প? অবশ্যই। `awk` একটু আরও উন্নত এবং কলাম এবং সারি নিয়ে চমক সৃষ্টি করতে পারে। দ্রুত মেরামতের জন্য, `grep` আপনাকে জিনিস খুঁজে পেতে সাহায্য করতে পারে, কিন্তু এটি প্রতিস্থাপন করবে না – এটি আরও বেশি পাহারা দেয়ার মতো।

অভ্যন্তরে, `sed` নিয়মিত এক্সপ্রেশন ব্যবহার করে, যা স্টেরয়েডে ওয়াইল্ডকার্ডের মতো। এগুলি প্রায় আপনি যেকোনো প্যাটার্নের সাথে ম্যাচ করতে পারে। এটি `sed` কে অত্যন্ত শক্তিশালী করে তোলে, কিন্তু এটি দখল করা একটু কঠিনও বটে।

## দেখুন এছাড়াও
- `sed` এর ম্যানুয়ালের জন্য `man sed`
- [`sed` এর পরিচিতি](https://www.gnu.org/software/sed/manual/sed.html)
- [নিয়মিত এক্সপ্রেশনস শুরুকারীদের জন্য](https://www.regular-expressions.info/tutorial.html)
- আরও ব্যাশ কৌশলের জন্য দ্য আর্ট অফ কমান্ড লাইন (https://github.com/jlevy/the-art-of-command-line)
