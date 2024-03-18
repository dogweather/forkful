---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:19.763960-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u099A\u09CD\u099B\u09C7 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\
  \u09B0 \u09AA\u09C1\u09A8\u09B0\u09CD\u0997\u09A0\u09A8\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u2014\u09AB\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09B0\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\u2014\
  \u09A4\u09AC\u09C7 \u098F\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\
  \u099A\u09B0\u09A3\u09C7 \u0995\u09CB\u09A8\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\
  \u09CD\u09A4\u09A8 \u09A8\u09BE \u0986\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.913192-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u099A\u09CD\u099B\u09C7 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\
  \u09B0 \u09AA\u09C1\u09A8\u09B0\u09CD\u0997\u09A0\u09A8\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u2014\u09AB\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09B0\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\u2014\
  \u09A4\u09AC\u09C7 \u098F\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\
  \u099A\u09B0\u09A3\u09C7 \u0995\u09CB\u09A8\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\
  \u09CD\u09A4\u09A8 \u09A8\u09BE \u0986\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
---

{{< edit_this_page >}}

## কি এবং কেন?
রিফ্যাক্টরিং হচ্ছে বিদ্যমান কম্পিউটার কোডের পুনর্গঠনের প্রক্রিয়া—ফ্যাক্টরিং পরিবর্তন—তবে এর বাহ্যিক আচরণে কোনো পরিবর্তন না আনার প্রক্রিয়া। প্রোগ্রামাররা এটি সফটওয়্যারের অ ফাংশনাল গুণাবলি, যেমন পঠনযোগ্যতা উন্নত করা, জটিলতা হ্রাস করা এবং ভবিষ্যত প্রকল্পের জন্য কোডকে আরও বজায় রাখার উপযোগী করে তোলা—করে থাকেন।

## কিভাবে:
চলুন আমরা একটি সহজ Java ক্লাস নিই যে তার খারাপ আয়োজন এবং স্পষ্টতার অভাবের জন্য রিফ্যাক্টরিংয়ের জন্য চিৎকার করছে।

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // অন্যান্য অপারেশন...
    }
}
```

রিফ্যাক্টরিং এর পরে, আমরা পেলাম:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // অন্যান্য অপারেশন...
}
```

রিফ্যাক্টরিং এর মাধ্যমে, আমরা পঠনযোগ্যতার জন্য পদ্ধতির নাম এবং প্যারামিটারগুলি উন্নত করেছি এবং একটি একক পদ্ধতিতে একটি শাখা-নির্ভর শর্ত থেকে এড়িয়ে গেছি। প্রতিটি অপারেশন এখন তার উদ্দেশ্য স্পষ্টভাবে উল্লেখ করে।

## গভীর ডুব:
রিফ্যাক্টরিং তার উৎস স্মলটক কমিউনিটিতে পেয়েছে, যার মূল মনোযোগ কোড পঠনযোগ্যতা এবং অবজেক্ট-ওরিয়েন্টেড ডিজাইনে ছিল, কিন্তু এটি সত্যিকার অর্থে Java জগতে ৯০-এর দশকের শেষ এবং ০০-এর দশকের শুরুতে, বিশেষ করে Martin Fowler-এর গুরুত্বপূর্ণ বই "Refactoring: Improving the Design of Existing Code" প্রকাশনার পরে উৎকর্ষ সাধিত হয়েছে।

কোড নতুন করে লেখার মতো রিফ্যাক্টরিং এর বিকল্প আছে। তবে, রিফ্যাক্টরিং প্রায়শই পছন্দ করা হয় কারণ এটি ক্রমিক পরিবর্তন জড়িত যা অ্যাপ্লিকেশনের কার্যকারিতাকে বিঘ্নিত করে না।

Java (অথবা যেকোনো প্রোগ্রামিং ভাষা) এ রিফ্যাক্টরিং করার সময় বাস্তবায়নের বিস্তারিত হল কোডের গন্ধ—কোডে গভীর সমস্যার নির্দেশকদের—বোঝা। কিছু গন্ধের মধ্যে রয়েছে দীর্ঘ পদ্ধতি, বড় ক্লাস, ডুপ্লিকেট কোড, এবং প্রিমিটিভের অতিরিক্ত ব্যবহার। Extract Method, Move Method, অথবা Replace Temp with Query এর মতো রিফ্যাক্টরিং প্যাটার্ন প্রয়োগ করে, ডেভেলোপাররা এই গন্ধগুলি সিস্টেমেটিকভাবে ঠিকানা দেয়, সর্বদা কোডকে ফাংশনাল রাখার নিশ্চয়তা দেয়।

IntelliJ IDEA-র রিফ্যাক্টরিং সাপোর্টের মতো অটোমেটেড টুল অথবা Eclipse-র প্লাগিনগুলি প্রক্রিয়াটি সাহায্য করতে পারে, যা ভেরিয়েবল, মেথড এবং ক্লাসের নাম পরিবর্তন, মেথড অথবা ভেরিয়েবল বের করে আনা, এবং মেথড অথবা ক্লাসকে বিভিন্ন প্যাকেজ বা নামস্থানে সরানোর মতো রিফ্যাক্টরিং অটোমেট করে।

## দেখুন এছাড়া:
- Martin Fowler এর "Refactoring: Improving the Design of Existing Code": https://martinfowler.com/books/refacto
