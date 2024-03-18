---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:18.083846-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\
  \u09B9\u09CD\u09A8 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE \u09A5\
  \u09C7\u0995\u09C7 \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u09A7\u09B0\u09A8\u09C7\
  \u09B0 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u099A\u09BF\u09B9\u09CD\u09A8\
  \u2014\u098F\u0995\u0995 (' '), \u09A6\u09CD\u09AC\u09C8\u09A4 (\" \"), \u0985\u09A5\
  \u09AC\u09BE \u0989\u09AD\u09AF\u09BC\u2014\u09B8\u09B0\u09BF\u09AF\u09BC\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.891576-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\
  \u09A8 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE \u09A5\u09C7\u0995\
  \u09C7 \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u09A7\u09B0\u09A8\u09C7\u09B0 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u099A\u09BF\u09B9\u09CD\u09A8\u2014\u098F\u0995\
  \u0995 (' '), \u09A6\u09CD\u09AC\u09C8\u09A4 (\" \"), \u0985\u09A5\u09AC\u09BE \u0989\
  \u09AD\u09AF\u09BC\u2014\u09B8\u09B0\u09BF\u09AF\u09BC\u09C7 \u09AB\u09C7\u09B2\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
একটি স্ট্রিং থেকে উদ্ধৃতিচিহ্ন মুছে ফেলা মানে টেক্সট ডেটা থেকে যেকোনো ধরনের উদ্ধৃতি চিহ্ন—একক (' '), দ্বৈত (" "), অথবা উভয়—সরিয়ে ফেলা। প্রোগ্রামাররা এটি ইনপুট পরিষ্কার করতে, ডেটা সংরক্ষণের জন্য প্রস্তুত করতে, অথবা পার্সিং কাজগুলো সহজ করতে করে থাকেন যেখানে উদ্ধৃতি চিহ্নগুলি অপ্রয়োজনীয় এবং সম্ভাব্য সমস্যার কারণ হতে পারে।

## কিভাবে:
আমাদের টেক্সট থেকে এই বিরক্তিকর উদ্ধৃতিচিহ্নগুলো টেনে বের করে ফেলি। আমরা দ্রুত সমাধানের জন্য `replace()` মেথড এবং কঠিন সমস্যার জন্য regex ব্যবহার করব।

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // এখন regex দিয়ে যারা প্যাটার্ন পছন্দ করেন তাদের জন্য
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## গভীরে ডুব দিন
আগের দিনগুলোতে, স্ট্রিংগুলোতে উদ্ধৃতিচিহ্ন এত বড় একটি বিষয় ছিল না—সিস্টেমগুলো সহজ ছিল এবং ডেটা এত জটিল ছিল না। জটিল ডেটা ফরম্যাটস (JSON, XML) এবং ডেটা এক্সচেঞ্জের প্রয়োজনের সাথে সাথে, উদ্ধৃতিচিহ্ন পরিচালনা একটি মুখ্য বিষয়ে পরিণত হয়েছে। বিকল্প সম্পর্কে বলতে গেলে, নিশ্চিতভাবে, আপনি একটি পার্সার লিখতে পারেন, প্রতিটি অক্ষরের মধ্য দিয়ে লুপ করতে পারেন, এবং একটি নতুন স্ট্রিং তৈরি করতে পারেন (বৃষ্টির দিনে মজা হতে পারে)। তৃতীয়-পক্ষের লাইব্রেরি গুলিও আছে যা এটি আরও জটিলভাবে সামলাতে পারে, অক্ষরগুলোকে মুছে ফেলার পরিবর্তে এসকেপ করার বিকল্প সরবরাহ করে, অথবা লোকেল অনুযায়ী বিভিন্ন ধরনের উদ্ধৃতি চিহ্ন সামলানো সক্ষম। বাস্তবায়নের দিক থেকে, মনে রাখবেন প্রসঙ্গ ছাড়াই উদ্ধৃতিচিহ্ন মুছে ফেলা ডেটার অর্থ বা গঠন পরিবর্তন করতে পারে—সবসময় "কেন" বিবেচনা করে "কিভাবে" নির্ধারণ করুন।

## আরও দেখুন
- রেগেক্স সম্পর্কে আরও গভীরে যেতে, অফিসিয়াল জাভা ডক্স দেখুন: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- উদ্ধৃতিচিহ্নগুলি মুছে ফেলার পরিবর্তে এসকেপ করতে চান? স্ট্যাক ওভারফ্লো আপনার পাশে আছে: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- জাভায় JSON প্রক্রিয়া করা? আপনি প্রায়শই উদ্ধৃতিচিহ্ন দেখতে পাবেন। এখানে একটি শুরুর পয়েন্ট: https://www.oracle.com/technical-resources/articles/java/json.html
