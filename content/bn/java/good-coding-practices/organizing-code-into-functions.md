---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:47.460850-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0995\u09CD\u09B2\u09BE\u09B8\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2 \u2014 \u098F\
  \u0995\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AB\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F\u09B0\u09BF\u09DF\u09BE\u09B2 \u0997\u09A3\u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u0964."
lastmod: '2024-03-17T18:47:43.910068-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09CD\u09B2\
  \u09BE\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09DF\u09BE \u09B9\u09B2 \u2014 \u098F\u0995\u099F\u09BF \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE\u09B0 \u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u09DF\u09BE\
  \u09B2 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\
  \u09A8\u0964."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কিভাবে:
এখানে একটি ক্লাসিক উদাহরণ দেওয়া হল — একটি সংখ্যার ফ্যাক্টরিয়াল গণনা করার ফাংশন।

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("সংখ্যা " + number + " এর ফ্যাক্টরিয়াল হল: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

আউটপুট হবে:
```
সংখ্যা 5 এর ফ্যাক্টরিয়াল হল: 120
```

## গভীর ডুব
ফাংশন চালু হওয়ার আগে, কোড বৃহত্তর ব্লকে সংকোচিত করা হত, যা ডিবাগিংকে একটি গোলা থেকে সূঁচ খোঁজার মত করে তুলেছিল। এখন, ফাংশনগুলিতে কার্যকারিতা আবদ্ধ করা সমস্যাগুলি দ্রুত পৃথক করার সাহায্য করে। বিকল্প উপায়ের মধ্যে জাভাতে ল্যাম্বডা এক্সপ্রেশন বা অবজেক্ট-অরিয়েন্টেড প্রোগ্রামিংয়ে মেথড অন্তর্ভুক্ত আছে, উভয়ই অনুরূপ উদ্দেশ্য পরিবেশন করে। আপনি যখন একটি ফাংশন লিখেন, মনে রাখবেন: (১) প্রতিটি ফাংশনের একটি একক দায়িত্ব থাকা উচিত এবং (২) ফাংশনের নাম এর উদ্দেশ্যকে স্পষ্টভাবে বর্ণনা করা উচিত।

## দেখুন আরও
কোড আয়োজন সম্পর্কে আরও জানার জন্য:
- ক্লিন কোড লেখক রবার্ট সি. মার্টিন
- রিফ্যাক্টরিং: বিদ্যমান কোডের ডিজাইন উন্নতি করা লেখক মার্টিন ফাউলার
- [অরাকল জাভা ডকস মেথড সংজ্ঞায়ন সম্পর্কে](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
