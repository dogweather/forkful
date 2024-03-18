---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:17:14.979643-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

জাভা স্ক্রিপ্টে টেক্সট অনুসন্ধান এবং প্রতিস্থাপন মূল স্ট্রিংগুলিকে নতুন অক্ষরের মাধ্যমে পুনঃলিখন করে - এটি ডিজিটাল হোয়াইট-আউট হিসেবে চিন্তা করা যেতে পারে। প্রোগ্রামাররা প্রায়শই এটি ডেটা পরিষ্কার, সেটিংস সংশোধন করা বা বার্তাগুলি কাস্টমাইজ করা জন্য ব্যবহার করে।

## কিভাবে:

জাভার মধ্যে `String` ক্লাস এবং এর `replace()` মেথডের সাহায্য করে টেক্সট অনুসন্ধান এবং প্রতিস্থাপন একদম সহজ। এখানে এটি কিভাবে করবেন:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String originalText = "The quick brown fox jumps over the lazy dog";
        String modifiedText = originalText.replace("lazy", "energetic");
        
        System.out.println("Before: " + originalText);
        System.out.println("After: " + modifiedText);
    }
}
```

আউটপুট:
```
Before: The quick brown fox jumps over the lazy dog
After: The quick brown fox jumps over the energetic dog
```

এখন, নিদর্শন বা জটিল প্রতিস্থাপনের জন্য, `Pattern` এবং `Matcher` কাজে আসে:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String originalText = "There are 31,536,000 seconds in 365 days.";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(originalText);
        String modifiedText = matcher.replaceAll("#");
        
        System.out.println("Before: " + originalText);
        System.out.println("After: " + modifiedText);        
    }
}
```

আউটপুট:
```
Before: There are 31,536,000 seconds in 365 days.
After: There are # seconds in # days.
```

## গভীর ডুব:

`replace()` মেথডটি জাভার প্রথম দিনগুলি থেকে তার উৎপত্তি মনে পড়ে। এটি অপরিবর্তনীয় `String` ক্লাসের একটি অংশ, যার অর্থ প্রতিবার আপনি এটি ব্যবহার করেন, আপনি একটি নতুন স্ট্রিং তৈরি করছেন। খুবই পরিবেশ বান্ধব, পুরানো জিনিসের কোনো অপচয় নেই।

তবে, `Pattern` এবং `Matcher` সম্পর্কে আপনি কি জিজ্ঞেস করছেন? এই ক্লাসগুলি জাভার রেগুলার এক্সপ্রেশন (regex) API-র অংশ, যা জাভা 1.4 এ প্রবর্তিত হয়েছিলো। তারা অনুসন্ধান এবং প্রতিস্থাপনে আরও শক্তি যোগ করে, জটিল নিদর্শন সনাক্ত করতে এবং টেক্সটকে গতিশীলভাবে সংশোধন করতে সাহায্য করে। এটি সেজ হাতুড়ির পরিবর্তে একটি স্ক্যালপেল ব্যবহার করার মতো।

তাছাড়া, `Matcher` ক্লাসের `replaceAll()` এবং `replaceFirst()`, দুটি মেথড আপনার টেক্সট ট্রান্সফরমেশনকে সূক্ষ্ণভাবে সাজায়, সমস্ত ঘটনা বা শুধুমাত্র প্রথম ম্যাচ প্রতিস্থাপন করে।

অন্য একটি বিকল্প হল যখন আপনি অসংখ্য সংশোধনগুলির সাথে মোকাবেলা করছেন তখন `StringBuffer` বা `StringBuilder` ক্লাস ব্যবহার করা, কারণ অনুরূপ `String`-এর বিপরীতে, এই বাফারগুলি পরিবর্তনযোগ্য।

## দেখুন:

- [Java String ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Pattern ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Matcher ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [রেগুলার এক্সপ্রেশন টিউটোরিয়াল](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

আরও হাতে কলমে অনুশীলনের জন্য, RegexOne (https://regexone.com) চেক আউট করুন, এটি আপনার regex দক্ষতা উন্নতি করার জন্য দারুণ একটি সম্পদ।
