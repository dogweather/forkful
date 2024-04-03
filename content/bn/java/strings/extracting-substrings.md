---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:29.737384-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09A4\
  \u09C7 \u0989\u09AA\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE \u0996\u09C1\u09AC\u0987 \u09B8\u09B0\u09B2 \u09AF\u09BE `substring`\
  \ \u09AE\u09C7\u09A5\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u0995\u09B0\u09BE \u09AF\u09BE\u09DF\u0964 \u098F\u099F\u09BE \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u09B9\u09B2\
  ."
lastmod: '2024-03-17T18:47:43.892630-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09A4\u09C7 \u0989\u09AA\u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u0996\u09C1\u09AC\u0987\
  \ \u09B8\u09B0\u09B2 \u09AF\u09BE `substring` \u09AE\u09C7\u09A5\u09A1 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09B0\u09BE \u09AF\u09BE\
  \u09DF\u0964 \u098F\u099F\u09BE \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\
  \u09AC\u09C7\u09A8 \u09A4\u09BE \u09B9\u09B2."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
জাভাতে উপস্ট্রিং বের করা খুবই সরল যা `substring` মেথড ব্যবহার করে করা যায়। এটা কিভাবে করবেন তা হল:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Hello, World!";

        // স্ট্রিংয়ের সাত নম্বর ইনডেক্স থেকে শেষ পর্যন্ত বের করে নাও
        String sub1 = fullString.substring(7);
        System.out.println(sub1); // আউটপুট: World!

        // স্ট্রিংয়ের শূন্য নম্বর ইনডেক্স থেকে চার নম্বর ইনডেক্স পর্যন্ত বের করে নাও (৫ অন্তর্ভুক্ত নয়)
        String sub2 = fullString.substring(0, 5);
        System.out.println(sub2); // আউটপুট: Hello
    }
}
```

**মনে রাখবেন**: জাভাতে, স্ট্রিং ইনডেক্সিং শূন্য থেকে শুরু হয়।

## গভীরে ডুব দেওয়া
`substring` মেথডটি জাভার প্রারম্ভিক দিনগুলি থেকে আছে, স্ট্রিংয়ের অংশ পেতে একটি সহজ উপায় প্রদান করে। পুরোনো জাভা ভার্সনগুলিতে, `substring` আসল চরিত্রের অ্যারে শেয়ার করত, যা আসল স্ট্রিং যদি বড় হয় এবং উপস্ট্রিং দীর্ঘ সময়ের জন্য রাখা হয়ে থাকে তবে মেমরি লিকে পরিণত হতে পারে। জাভা 7 আপডেট 6 থেকে, `substring` একটি নতুন স্ট্রিং তৈরি করে, সুতরাং পুরোনোটি যদি অন্যখানে ব্যবহৃত না হয় তবে তা মুছে ফেলা যায়।

আরও, `substring` ব্যবহারের আগে, আপনি যদি `split`, `replace`, অথবা রেগুলার এক্সপ্রেশন ইউটিলিটিগুলি ব্যবহার করতে চান ভাবুন কারণ জটিল পরিস্থিতিগুলোর জন্য এগুলি উত্তম। অভ্যন্তরীণভাবে, জাভাতে `substring` `String` ক্লাসের মেথড ব্যবহার করে যা অ্যারেগুলি কপি করে—দক্ষ, কিন্তু সেটি আপনার সরাসরি নিয়ন্ত্রণে নেই।

## আরও দেখুন
- জাভাতে স্ট্রিং নিয়ে আপনি যা কিছু করতে পারেন তার সম্পূর্ণ চিত্র পেতে, `String` ক্লাসের ডকুমেন্টেশন দেখুন: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- জটিল স্ট্রিং ম্যানিপুলেশনে গভীরে ডুব দিতে চাইলে, `Pattern` এবং `Matcher` ক্লাসগুলি আপনার বন্ধু: [Pattern (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html)
- জাভাতে রেগুলার এক্সপ্রেশন ব্যবহার সম্পর্কিত একটি টিউটোরিয়াল: [Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)

এটা একটা দ্রুত ছাঁটাই অথবা একটা জটিল ডেটা প্রত্যাহারের জন্য হোক, আপনার প্রয়োজনীয় ফাংশনগুলি সেখানে আছে। আপনার টুলকিটকে ভালোভাবে বুঝে নিয়ে প্রস্তুত রাখুন।
