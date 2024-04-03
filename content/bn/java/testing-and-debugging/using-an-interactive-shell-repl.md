---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:51.018643-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Java 9-\u098F \u09AA\u09CD\u09B0\
  \u09AC\u09B0\u09CD\u09A4\u09BF\u09A4 `jshell` \u099F\u09C1\u09B2\u09C7\u09B0 \u09B8\
  \u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 Java-\u09A4\u09C7 REPL \u09B6\u09C1\u09B0\
  \u09C1 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BF\u09B0 \u0989\u09AA\u09B0\
  \ \u09B9\u09BE\u09A4 \u09AA\u09C7\u09A4\u09C7 \u098F\u09AC\u0982 \u098F\u0995\u099F\
  \u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u09B8\u09C7\u09B6\u09A8 \u09B6\u09C1\u09B0\
  \u09C1 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AF\u09BC."
lastmod: '2024-03-17T18:47:43.906052-06:00'
model: gpt-4-0125-preview
summary: "Java 9-\u098F \u09AA\u09CD\u09B0\u09AC\u09B0\u09CD\u09A4\u09BF\u09A4 `jshell`\
  \ \u099F\u09C1\u09B2\u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7\
  \ Java-\u09A4\u09C7 REPL \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09B8\u09B9\
  \u099C\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u098F\u099F\u09BF\u09B0 \u0989\u09AA\u09B0 \u09B9\u09BE\u09A4 \u09AA\u09C7\u09A4\
  \u09C7 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u09B8\u09C7\u09B6\u09A8 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7 \u09B9\
  \u09AF\u09BC."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

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
