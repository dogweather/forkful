---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:08.699885-06:00
description: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\
  \u09B2 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u099C\u09A8\u09CD\
  \u09AF \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09AD\u09C7\u09A4\u09B0\u09C7\u09B0\
  \ \u09A1\u09C7\u099F\u09BE \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\u09A4\u09BF\u09B0\
  \ \u099C\u09A8\u09CD\u09AF, \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF, \u09B2\u0997 \u09AA\u09BE\u09B0\u09CD\u09B8\u2026"
lastmod: '2024-03-17T18:47:44.515822-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u099C\u09A8\u09CD\
  \u09AF \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09AD\u09C7\u09A4\u09B0\u09C7\u09B0\
  \ \u09A1\u09C7\u099F\u09BE \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\u09A4\u09BF\u09B0\
  \ \u099C\u09A8\u09CD\u09AF, \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF, \u09B2\u0997 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\
  \u09B0\u09BE \u09AC\u09BE \u0995\u09C7\u09AC\u09B2 \u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09C7 \u09A1\u09C7\u099F\u09BE \u09B8\u09B0\u09AC\u09B0\
  \u09BE\u09B9\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
এখানে ফিশ শেল দিয়ে টেক্সট ফাইল খোলার প্রক্রিয়া অবলম্বন করা হল:

```Fish Shell
# একটি ফাইলকে লাইন অনুযায়ী পড়ুন
while read -la line
    echo $line
end < file.txt
```

```Fish Shell
# সরাসরি একটি ফাইলের সামগ্রী প্রদর্শন
cat file.txt
```

নমুনা আউটপুট (`cat` থেকে):

```plaintext
Hello, Fish!
Just swimming through files.
```

## গভীর ভাবনা
একসময়, ফিশ শেল তার আবির্ভাব সার্কা ২০০৫ এর আগে, ফাইল পড়া একটি অবশ্যপালনীয় কাজ ছিল। ইউনিক্স শেলগুলি সবসময় এর জন্য টুলস সম্বলিত ছিল। কেন ফিশ? এটি বন্ধুসুলভ, আধুনিক এবং স্ক্রিপ্টিং ডিফল্টসমূহে যুক্তিসঙ্গত, পুরানো শেলগুলির তুলনায় এটি একটি আনন্দদায়ক বিকল্প করে তোলে।

`while read` লুপটি লাইন অনুযায়ী সামান্য পরিবর্তনের জন্য উপকারী। মনে রাখবেন যে, `read` এর `-la` মত ফ্ল্যাগ আছে যা লাইন থেকে লিস্ট ভেরিয়েবল তৈরি করে — যা কমা দ্বারা পৃথকীকৃত মানের জন্য দারুণ।

অন্যদিকে, `cat` খুবই সরল। এটি ফাইলের সামগ্রীগুলি একত্রিত করে এবং প্রদর্শন করে। এটি ইউনিক্সে ১৯৭১ সাল থেকে (ভালো, অনেকটা সময় ধরে) রয়েছে।

পারফরম্যান্সের দিক থেকে, সরাসরি পঠন সাধারণত দ্রুততর এবং ছোট ফাইলগুলির জন্য ঠিক আছে। কিন্তু যখন আপনি মোবি ডিকের মতো আকারের টেক্সট ফাইল নিয়ে কাজ করবেন, তখন লাইন অনুযায়ী প্রক্রিয়া বা `sed`, `awk` বা এমনকি `grep` এর মতো টুলস বিবেচনা করুন যদি আপনি নির্দিষ্ট লাইনের জন্য মাছ ধরতে চান।

## আরও দেখুন
- ফিশ শেল সম্পর্কিত সব কিছুর গভীর অনুধাবনের জন্য [অফিসিয়াল ফিশ ডকুমেন্টেশন](https://fishshell.com/docs/current/index.html)।
- প্রশস্ত কমিউনিটি সমর্থন এবং অন্তর্দৃষ্টির জন্য একটি [ইউনিক্স স্ট্যাক এক্সচেঞ্জ থ্রেড](https://unix.stackexchange.com/questions/tagged/fish)।
- যদি আরও জটিল টেক্সট প্রক্রিয়াজনীত কাজ উঠে আসে তবে শেল স্ক্রিপ্টিংয়ে [awk ব্যবহার করা সম্পর্কিত একটি টিউটোরিয়াল](https://www.gnu.org/software/gawk/manual/gawk.html) কার্যকরী হতে পারে।
