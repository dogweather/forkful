---
title:                "স্ট্রিং এর প্রথম অক্ষর বড় হাতের করা"
date:                  2024-03-17T17:45:41.880154-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
একটি স্ট্রিং-এর প্রথম অক্ষরকে বড় হাতের (uppercase) অক্ষরে পরিণত করে এবং বাকি অক্ষরগুলিকে ছোট হাতের (lowercase) অক্ষরে রেখে দেওয়াই হল স্ট্রিং-কে ক্যাপিটালাইজ করার প্রক্রিয়া। এই সাধারণ স্ট্রিং ম্যানিপুলেশন কাজটি অ্যাপ্লিকেশনগুলিতে টেক্সট ফর্ম্যাটিং এর জন্য উপযোগী, যেমন ব্যবহারকারীর নাম বা শিরোনামগুলি ঐতিহ্য অনুযায়ী বা ব্যাকরণিক সঠিকতার জন্য প্রস্তুত করা।

## কিভাবে:
জাভার স্ট্যান্ডার্ড লাইব্রেরি একবারে সম্পূর্ণ স্ট্রিং ক্যাপিটালাইজ করার জন্য সরাসরি কোন পদ্ধতি প্রদান করে না, তবে আপনি বিল্ট-ইন পদ্ধতিগুলির সমন্বয়ে এটি সম্পন্ন করতে পারেন। আরও জটিল প্রয়োজনের জন্য, থার্ড-পার্টি লাইব্রেরি যেমন Apache Commons Lang সরাসরি সমাধান অফার করে।

### জাভার বিল্ট-ইন পদ্ধতি ব্যবহার করে
বাইরের লাইব্রেরিগুলি ছাড়াই একটি স্ট্রিং ক্যাপিটালাইজ করার জন্য, আপনি স্ট্রিংটি শব্দে বিভক্ত করতে পারেন, প্রতিটির প্রথম অক্ষরটি বড় হাতের অক্ষরে পরিণত করতে পারেন এবং তারপর তাদেরকে আবার যোগ করতে পারেন। এখানে একটি সহজ প্রক্রিয়া:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // আউটপুট: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

এই কোড স্নিপেটটি পুরো স্ট্রিংটিকে ছোট হাতের অক্ষরে রূপান্তরিত করে, তারপর প্রতিটি অক্ষরের মাধ্যমে ইটারেট করে এবং প্রতিটি শব্দের প্রথম অক্ষরটি বড় হাতের অক্ষরে পরিণত করে। এটি শব্দ বিভাজক হিসেবে স্পেস, দাঁড়ি এবং অপস্ট্রফিগুলিকে বিবেচনা করে।

### Apache Commons Lang ব্যবহার করে

Apache Commons Lang লাইব্রেরি `WordUtils.capitalizeFully()` পদ্ধতি দিয়ে আরও সুন্দর সমাধান প্রদান করে, যা বিভিন্ন এজ কেস এবং ডেলিমিটার গুলির জন্য আপনাকে সাহায্য করে:

```java
// ডিপেনডেন্সি যোগ করুন: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // আউটপুট: "Hello, World!"
    }
}
```

এই পদ্ধতিটি ব্যবহার করতে, আপনার প্রজেক্টে Apache Commons Lang লাইব্রেরিটি যোগ করতে হবে। এই লাইব্রেরি পদ্ধতিটি না শুধুমাত্র প্রতিটি শব্দের প্রথম অক্ষরকে বড় হাতের অক্ষরে পরিণত করে না, এছাড়াও প্রতিটি শব্দের বাকি অক্ষরগুলি ছোট হাতের অক্ষরে পরিণত করে, সারা স্ট্রিং জুড়ে এক ধরনের ক্যাপিটালাইজেশন প্যাটার্ন নিশ্চিত করে।
