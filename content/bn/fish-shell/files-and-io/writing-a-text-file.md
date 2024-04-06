---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:50.659346-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish \u098F \u098F\u0995\u099F\
  \u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF `echo`\
  \ \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B0\
  \u09BF\u09A1\u09BE\u0987\u09B0\u09C7\u0995\u09B6\u09A8 \u0985\u09AA\u09BE\u09B0\u09C7\
  \u099F\u09B0\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AB\u09BE\u0987\
  \u09B2 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF Fish-\u098F \u09AC\
  \u09BF\u09B6\u09C7\u09B7 \u0995\u09CB\u09A8\u09CB\u2026"
lastmod: '2024-04-05T21:53:53.202487-06:00'
model: gpt-4-0125-preview
summary: "Fish \u098F \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u0986\u09AA\u09A8\u09BF `echo` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B0\u09BF\u09A1\u09BE\u0987\u09B0\u09C7\u0995\
  \u09B6\u09A8 \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0\u0997\u09C1\u09B2\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF Fish-\u098F \u09AC\u09BF\u09B6\u09C7\u09B7 \u0995\u09CB\
  \u09A8\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\
  \u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC \u09A8\u09AF\u09BC, \u0995\u09BE\u09B0\u09A8 \u09B6\
  \u09C7\u09B2\u09C7\u09B0 \u09A8\u09BF\u099C\u09B8\u09CD\u09AC \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1\u0997\u09C1\u09B2\u09BF \u098F\u0987 \u0989\u09A6\u09CD\u09A6\
  \u09C7\u09B6\u09CD\u09AF\u09C7 \u09B8\u09CB\u099C\u09BE \u098F\u09AC\u0982 \u09A6\
  \u0995\u09CD\u09B7\u0964."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
Fish এ একটি টেক্সট ফাইলে লেখার জন্য, আপনি `echo` কমান্ডের সাথে রিডাইরেকশন অপারেটরগুলি ব্যবহার করতে পারেন। ফাইল লেখার জন্য Fish-এ বিশেষ কোনো তৃতীয়-পক্ষের লাইব্রেরি জনপ্রিয় নয়, কারন শেলের নিজস্ব কমান্ডগুলি এই উদ্দেশ্যে সোজা এবং দক্ষ।

### নতুন ফাইলে টেক্সট লেখা অথবা বিদ্যমান ফাইলের উপরে লেখা:
```fish
echo "Hello, Fish Shell!" > output.txt
```
এই কমান্ডটি "Hello, Fish Shell!" কে `output.txt` এ লিখে, যদি ফাইলটি না থাকে তবে তা তৈরি করে বা যদি থাকে তবে তা ওভাররাইট করে।

### বিদ্যমান ফাইলে টেক্সট যোগ করা:
যদি আপনি একটি বিদ্যমান ফাইলের শেষে টেক্সট যোগ করতে চান এবং এর বর্তমান কন্টেন্ট সরিয়ে না দিতে চান, তবে আপেন্ড অপারেটর `>>` ব্যবহার করুন:
```fish
echo "Adding new line to file." >> output.txt
```

### একাধিক লাইন লেখা:
আপনি একটি ফাইলে একাধিক লাইন লিখতে চাইলে echo কমান্ডের সাথে নিউলাইন ক্যারেক্টার `\n` ব্যবহার করতে পারেন, অথবা আপনি বেশ কয়েকটি echo কমান্ডকে সেমিকোলন ব্যবহার করে চেইন করতে পারেন:
```fish
echo "First Line\nSecond Line" > output.txt
# OR
echo "First Line" > output.txt; echo "Second Line" >> output.txt
```

### নমুনা আউটপুট:
`output.txt`-এর কন্টেন্ট উপরের কমান্ডগুলি চালানোর পরে দেখার জন্য, `cat` কমান্ড ব্যবহার করুন:
```fish
cat output.txt
```
```plaintext
First Line
Second Line
```
উপরে দেখানো পদ্ধতিতে টেক্সট প্রতিস্থাপন বা যোগ করে আপনার প্রয়োজন অনুসারে ফাইলের কন্টেন্টকে পরিবর্তন করা হয়েছে, যা Fish Shell-এ টেক্সট ফাইলের সাথে কাজ করার সহজ তবে শক্তিশালী উপায় দেখায়।
