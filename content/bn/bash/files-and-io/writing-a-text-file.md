---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:27.500021-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Bash \u09AB\u09BE\u0987\u09B2\u09C7\
  \ \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09CB\u099C\u09BE\
  \u09B8\u09BE\u09AA\u09CD\u099F\u09BE \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09B8\u09B0\u09CD\u09AC\
  \u09BE\u09A7\u09BF\u0995 \u09AA\u09CD\u09B0\u099A\u09B2\u09BF\u09A4 \u09B9\u09B2\
  \ \u09B0\u09BF\u09A1\u09BE\u0987\u09B0\u09C7\u0995\u09B6\u09A8 \u0985\u09AA\u09BE\
  \u09B0\u09C7\u099F\u09B0 (`>`, `>>`) \u098F\u09AC\u0982 `tee` \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0989\u09AD\u09AF\u09BC\u2026"
lastmod: '2024-03-17T18:47:44.246400-06:00'
model: gpt-4-0125-preview
summary: "Bash \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09B8\u09CB\u099C\u09BE\u09B8\u09BE\u09AA\u09CD\u099F\u09BE\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7\u0964 \u09B8\u09B0\u09CD\u09AC\u09BE\u09A7\u09BF\u0995 \u09AA\u09CD\
  \u09B0\u099A\u09B2\u09BF\u09A4 \u09B9\u09B2 \u09B0\u09BF\u09A1\u09BE\u0987\u09B0\
  \u09C7\u0995\u09B6\u09A8 \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0 (`>`, `>>`)\
  \ \u098F\u09AC\u0982 `tee` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0989\u09AD\u09AF\u09BC \u0995\u09CC\u09B6\u09B2 \u09A6\u09C7\u0996\u09BE\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u09B8\u09BE\u09B0\u09BE\
  \u0982\u09B6 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964\n\n\u09B0\u09BF\
  \u09A1\u09BE\u0987\u09B0\u09C7\u0995\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\
  \u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09B2\
  \u09C7\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 `>` \u0985\u09AA\u09BE\
  \u09B0\u09C7\u099F\u09B0 \u0995\u09CB\u09A8\u09CB \u09AB\u09BE\u0987\u09B2\u09C7\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B2\u09C7\u0996\u09C7, \u09AF\
  \u09A6\u09BF \u09A4\u09BE \u0987\u09A4\u09BF\u09AE\u09A7\u09CD\u09AF\u09C7 \u09AC\
  \u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8 \u09A5\u09BE\u0995\u09C7 \u09A4\u09AC\
  \u09C7 \u09A4\u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8 \u0995\u09B0\u09C7, \u09AF\u0996\u09A8 `>>` \u0995\u09CB\u09A8\
  \u09CB \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8 \u09AB\u09BE\u0987\u09B2\
  \u09C7 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C7, \u09A4\u09BE\u09B0 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\
  \ \u09AE\u09C1\u099B\u09C7 \u09A6\u09C7\u09AC\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AF\
  \u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987\u0964."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
Bash ফাইলে লেখার জন্য সোজাসাপ্টা পদ্ধতি প্রদান করে। সর্বাধিক প্রচলিত হল রিডাইরেকশন অপারেটর (`>`, `>>`) এবং `tee` কমান্ড ব্যবহার করা। এখানে উভয় কৌশল দেখার একটি দ্রুত সারাংশ দেওয়া হল।

রিডাইরেকশন ব্যবহার করে, আপনি সরাসরি ফাইলে আউটপুট লেখতে পারেন। `>` অপারেটর কোনো ফাইলে কন্টেন্ট লেখে, যদি তা ইতিমধ্যে বিদ্যমান থাকে তবে তাকে প্রতিস্থাপন করে, যখন `>>` কোনো বিদ্যমান ফাইলে কন্টেন্ট যোগ করে, তার কন্টেন্ট মুছে দেবার প্রয়োজন ছাড়াই।

```bash
# > ব্যবহার করে ফাইলে লেখা
echo "Hello, World!" > myfile.txt

# >> ব্যবহার করে ফাইলে যোগ করা 
echo "This is a new line." >> myfile.txt
```

উপরের কমান্ড রান করার পরে আপনি যদি `myfile.txt`-এর কন্টেন্ট চেক করেন, তাহলে দেখতে পাবেন:

```
Hello, World!
This is a new line.
```

যখন আপনি ফাইলে লেখার পাশাপাশি স্ক্রিনেও (stdout) আউটপুট দেখতে চান তখন `tee` কমান্ড খুব কার্যকরী। ডিফল্ট হিসেবে, `tee` ফাইলটি প্রতিস্থাপন করে, কিন্তু `-a` ফ্ল্যাগ ব্যবহার করলে, এটি ফাইলে যোগ করে।

```bash
# tee ব্যবহার করে লেখা এবং প্রদর্শন
echo "Hello, again!" | tee myfile.txt

# tee -a ব্যবহার করে যোগ এবং প্রদর্শন
echo "Adding another line." | tee -a myfile.txt
```

এগুলো রান করার পর, `myfile.txt` দেখাবে:

```
Hello, again!
Adding another line.
```

যদিও Bash নিজেই রিডাইরেকশন এবং `tee` এর মতো কমান্ড ব্যবহার করে ফাইল ম্যানিপুলেশনের ক্ষমতা প্রদান করে, তবুও অধিক জটিল স্থিতিগুলির জন্য এক্সটার্নাল টূল বা স্ক্রিপ্টিং ভাষাগুলি (যেমন, Awk, Sed, Python) ডাকতে পারে, যা আরও উন্নত টেক্সট প্রসেসিং ফাংশন প্রদান করে। তবে, সাধারণ ফাইল লেখার কাজের জন্য, উপরের পদ্ধতিগুলি সম্পূর্ণ পর্যাপ্ত এবং ব্যাপকভাবে ব্যবহৃত হয়।
