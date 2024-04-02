---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:51.018643-06:00
description: "REPL (Read-Eval-Print Loop) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2\
  \ \u09AF\u09BE \u098F\u0995\u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\
  \u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09C7, \u0995\u09CB\u09A1 \u09A8\
  \u09BF\u09B0\u09CD\u09AC\u09BE\u09B9 \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\
  \u09B2\u09BE\u09AB\u09B2 \u09AB\u09C7\u09B0\u09A4 \u09AA\u09BE\u09A0\u09BE\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:43.906052-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2\
  \ \u09AF\u09BE \u098F\u0995\u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\
  \u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09C7, \u0995\u09CB\u09A1 \u09A8\
  \u09BF\u09B0\u09CD\u09AC\u09BE\u09B9 \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\
  \u09B2\u09BE\u09AB\u09B2 \u09AB\u09C7\u09B0\u09A4 \u09AA\u09BE\u09A0\u09BE\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE\u2026"
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কি এবং কেন?
REPL (Read-Eval-Print Loop) হল একটি ইন্টারেক্টিভ শেল যা একক ব্যবহারকারীর ইনপুট প্রক্রিয়া করে, কোড নির্বাহ করে, এবং ফলাফল ফেরত পাঠায়। প্রোগ্রামাররা তাৎক্ষণিক ফিডব্যাক এবং ইটারেশন সম্ভব করে তোলায়, দ্রুত পরীক্ষা, ডিবাগিং অথবা শিখতে গেলে এটি ব্যবহার করে।

## কিভাবে:
Java 9-এ প্রবর্তিত `jshell` টুলের সাহায্যে Java-তে REPL শুরু করা সহজ। এখানে কিভাবে এটির উপর হাত পেতে এবং একটি বেসিক সেশন শুরু করতে হয়:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  পদ্ধতি তৈরি হয়েছে sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

যেকোন সময় `/exit` দিয়ে বের হোন।

```Java
jshell> /exit
|  বিদায়
```

## গভীর ডুব
`jshell`-এর আগে, Java প্রোগ্রামাররা পাইথন অথবা রুবি ডেভেলপারদের মত একটি অফিসিয়াল REPL পেত না। তারা IDE ব্যবহার করত অথবা তুচ্ছ কাজের জন্যও পূর্ণ প্রোগ্রাম লিখত। Java 9 এ `jshell` একটি খেলাধুলার মত পরিবর্তন এনেছিল, সেই ফাঁক পূরণ করে।

বিকল্পগুলি অনলাইন কম্পাইলার বা IDE প্লাগইনস রয়েছে, তবে তারা `jshell`-এর তাৎক্ষণিকতার সাথে মিল রাখতে পারেনি। অভ্যন্তরে, `jshell` জাভা কম্পাইলার API ব্যবহার করে কোড খণ্ড নির্বাহ করে, যা বেশ চমৎকার। এটি কেবল একটি খেলার মাঠ নয়—এটি লাইব্রেরি আমদানি করতে, ক্লাস সংজ্ঞায়িত করতে এবং আরও অনেক কিছু করতে পারে। এটি প্রোটোটাইপিংয়ের জন্য একটি দৃঢ় টুল করে তোলে।

## দেখুন
- [JShell ব্যবহারকারীর গাইড](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java প্ল্যাটফর্ম, স্ট্যান্ডার্ড এডিশন টুলস রেফারেন্স](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java কম্পাইলার API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
