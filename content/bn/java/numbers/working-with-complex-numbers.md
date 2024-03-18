---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:48.502647-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\
  \u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u098F\u0995\
  \u0995, `i`, \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\
  \u09AC \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B0\u09C7\u0996\u09BE\u0995\
  \u09C7 \u09AC\u09BF\u09B8\u09CD\u09A4\u09C3\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\
  \u0996\u09BE\u09A8\u09C7 `i^2 = -1`\u0964 \u09A4\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\
  \u0995\u09CC\u09B6\u09B2, \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\u09BF\u09A6\
  \u09CD\u09AF\u09BE \u098F\u09AC\u0982 \u0989\u09A8\u09CD\u09A8\u09A4 \u0997\u09A3\
  \u09BF\u09A4\u09C7\u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-03-17T18:47:43.898069-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u098F\u0995\u0995\
  , `i`, \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B0\u09C7\u0996\u09BE\u0995\u09C7\
  \ \u09AC\u09BF\u09B8\u09CD\u09A4\u09C3\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u0996\
  \u09BE\u09A8\u09C7 `i^2 = -1`\u0964 \u09A4\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\u0995\
  \u09CC\u09B6\u09B2, \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\u09BF\u09A6\u09CD\
  \u09AF\u09BE \u098F\u09AC\u0982 \u0989\u09A8\u09CD\u09A8\u09A4 \u0997\u09A3\u09BF\
  \u09A4\u09C7\u09B0 \u09AE\u09A4\u09CB\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

জটিল সংখ্যা একটি কাল্পনিক একক, `i`, যোগ করে বাস্তব সংখ্যার রেখাকে বিস্তৃত করে, যেখানে `i^2 = -1`। তারা প্রকৌশল, পদার্থবিদ্যা এবং উন্নত গণিতের মতো ক্ষেত্রগুলিতে অপরিহার্য, যেখানে তারা বাস্তব সংখ্যা দ্বারা সামলানো যায় না এমন ঘটনা, যেমন বৈদ্যুতিক প্রবাহ এবং সিগন্যাল প্রক্রিয়াকরণ মডেল করে।

## কিভাবে:

Java জটিল সংখ্যার জন্য অন্তর্নিহিত সমর্থন রাখে না, কিন্তু আমরা নিজেরাই একটি ক্লাস তৈরি করতে পারি অথবা একটি লাইব্রেরি ব্যবহার করতে পারি। এখানে একটি সহজ `ComplexNumber` ক্লাস তৈরি এবং ব্যবহার করার একটি দ্রুত উদাহরণ দেওয়া হলো:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString জটিল সংখ্যাগুলি a + bi ফরম্যাটে প্রদর্শন করার জন্য
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // দ্রুত পরীক্ষা
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("সম: " + c1.add(c2));
    }
}
```

মেইন পদ্ধতির জন্য নমুনা আউটপুট হবে:

```
সম: 3.0 + 7.0i
```

## গভীর ডাইভ

Java এর মতো উচ্চ-স্তরের ভাষাগুলোর আগে, প্রোগ্রামাররা Fortran অথবা C এর মতো ভাষাগুলোতে সরাসরি গণিত লাইব্রেরিগুলির সাথে কাজ করতো জটিল অপারেশনগুলি ম্যানেজ করার জন্য। ধারণাটি 16 শতাব্দীতে গেরোলামো কার্ডানো এবং রাফাএল বোম্বেলির মতো গণিতবিদদের কাছে কৃতিত্ব পায়।

Java তে, `java.lang.Math` অপরিহার্যতার জন্য একটি যাওয়া-আসা স্থান কিন্তু জটিল সংখ্যাগুলি উপেক্ষা করে, সম্ভবত কারণ প্রতিটি প্রোগ্রামার তাদের ব্যবহার করে না। বিকল্প? লাইব্রেরি ব্যবহার করা। Apache Commons Math একটি `Complex` ক্লাস প্রদান করে যা পদ্ধতি পূর্ণ ভরা ম্যানিপুলেশনের জন্য। নিজের একটি ক্লাস বানানোর জন্য এটি দারুণ কেন: হালকা, আপনার নির্দিষ্ট প্রয়োজনে অভিযোজিত, এবং কোনো লাইব্রেরি ওভারহেড নেই।

একটি গুরুত্বপূর্ণ বিবরণ: ভাসমান-বিন্দু নির্ভুলতার জন্য সতর্ক থাকুন। কম্পিউটার কিছু সংখ্যা ঠিকভাবে প্রকাশ করতে পারে না, ফলে বৃত্তাকার ত্রুটি সৃষ্টি হয়। পুনরাবৃত্তি জটিল অপারেশনগুলি সঞ্চালনের সময়, এই ত্রুটিগুলি জমা হতে পারে!

## আরো দেখুন

আরও গভীর ডাইভ এবং জটিল অপারেশনের জন্য, দেখুন:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience এর Complex ক্লাস](http://jscience.org/)
- Oracle এর টিউটোরিয়াল সমূহ [ভাসমান-বিন্দু অঙ্কগণিতে](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
