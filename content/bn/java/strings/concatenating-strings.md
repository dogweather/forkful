---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:40.371791-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09A4\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u099C\
  \u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC\u09C7 \u09A8\u09BF\
  \u099A\u09C7\u09B0 \u09A4\u09A5\u09CD\u09AF\u09BE\u09AC\u09B2\u09C0."
lastmod: '2024-03-17T18:47:43.895994-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09A4\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A8\u09BF\u09B0\u09CD\
  \u09A3\u09AF\u09BC\u09C7 \u09A8\u09BF\u099A\u09C7\u09B0 \u09A4\u09A5\u09CD\u09AF\
  \u09BE\u09AC\u09B2\u09C0."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
জাভাতে স্ট্রিংগুলি জোড়া দেওয়ার উপায় নির্ণয়ে নিচের তথ্যাবলী:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "জন";
        String lastName = "ডো";
        
        // প্লাস অপারেটর ব্যবহার করে
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // আউটপুট: জন ডো
        
        // concat() মেথড ব্যবহার করে
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // আউটপুট: জন ডো
        
        // একাধিক জোড়ার জন্য StringBuilder ব্যবহার 
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // আউটপুট: জন ডো
    }
}
```

## বিস্তারিত আলোচনা
স্ট্রিংগুলি জোড়া দেওয়া যথেষ্ট সহজ মনে হচ্ছে, তাই না? এটি জাভাতে শুরু থেকেই ছিল, এবং আমাদের কাছে এটি করার কয়েকটি উপায় আছে। প্রাথমিক জাভা সংস্করণগুলিতে `+` ব্যবহার করার সময় অধীনে StringBuilder ব্যবহৃত হতো। তারপর জাভা 5 এসেছিল, এবং `StringJoiner` এর পরিচয় এবং `StringBuilder` শ্রেণির আরও উন্নতির সাথে জিনিসগুলি আরও দক্ষ হয়ে উঠলো।

এখন, আপনি হয়তো ভাবছেন যে `+` অপারেটর সবসময় ব্যবহার করা যায় না কেন, যদি এটি একই জিনিস হয়? মূলত, `+` ছোট স্ট্রিংগুলির জন্য অথবা কয়েকটি জোড়ার জন্য দ্রুত কাজের জন্য দুর্দান্ত। কিন্তু, ব্যাকগ্রাউন্ডে, যদি আপনি অনেকগুলি পুনরাবৃত্তির সাথে একটি লুপে এটি ব্যবহার করেন তবে এটি কর্মক্ষমতার দিক থেকে খরচে পরিণত হতে পারে কারণ চূড়ান্ত স্ট্রিং সংস্করণে পৌঁছানোর আগে এটি অস্থায়ী অবজেক্ট তৈরি করে।

এই ধরনের চাপের কাজের জন্য , `StringBuilder` অথবা `StringBuffer` আত্মপ্রকাশ করে। `StringBuilder` সাধারণত দ্রুততর কারণ এটি সিঙ্ক্রোনাইজড নয়— অর্থাৎ এটি থ্রেড-আনসেইফ কিন্তু দ্রুত। `StringBuffer` পুরানো, থ্রেড-সেইফ বিকল্প। এটি সিঙ্ক্রোনাইজেশন ওভারহেডের কারণে ধীর। আপনার থ্রেড নিরাপত্তার প্রয়োজন অনুসারে চয়ন করুন।

`concat()` মেথডের জন্য, এটি সরল কিন্তু `StringBuilder` এর মত নমনীয় নয়। লুপ করে আরও অধিক স্ট্রিং যোগ করতে চান? `concat()` কম সুবিধাজনক।

জাভা 8 এবং তার পরে, আমাদের কাছে `String.join()` আছে যা ডেলিমিটারের সাথে স্ট্রিংয়ের সংগ্রহ যোগ করার জন্য বেশ চমৎকার।

## আরও দেখুন
- [স্ট্রিং ক্লাসের ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [StringBuilder ক্লাসের ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [StringBuffer ক্লাসের ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [জাভার অরাকল টিউটোরিয়ালস অন স্ট্রিংস](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
