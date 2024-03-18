---
title:                "এলোমেলো সংখ্যা উৎপন্ন করা"
date:                  2024-03-17T17:51:26.280117-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

যাদুঘর সংখ্যা নির্মাণ করা মূলত নির্ধারিত পরিসরে অনুমান করা যায় এমন ক্রম বা একক মান উৎপন্ন করার প্রক্রিয়া। প্রোগ্রামারগণ বিভিন্ন কারণে এই কৌশলটি ব্যবহার করে থাকেন, যেমন সিমুলেশন, গেমস, নিরাপত্তা অ্যাপ্লিকেশন, এবং ভিন্ন ভিন্ন শর্ত অধীনে অ্যালগোরিদম পরীক্ষা করার জন্য নমুনা পদ্ধতিতে।

## কিভাবে:

Java-এ, যাদুঘর সংখ্যা নির্মাণ করা সম্ভব `Random` শ্রেণীটি `java.util` প্যাকেজ থেকে, অথবা `ThreadLocalRandom` এবং `SecureRandom` ক্লাসগুলো বিশেষ ব্যবহারের ক্ষেত্রে। নিম্নলিখিত উদাহরণগুলি এই ক্লাসগুলো ব্যবহার করার পদ্ধতি দেখায়।

### `Random` ক্লাস ব্যবহার করা
`Random` ক্লাস সৃজনশীল যাদুঘর সংখ্যা উৎপন্ন করার একটি উপায় প্রস্তাব করে।

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // একটি Random অবজেক্ট তৈরি করুন

        int randInt = rand.nextInt(50); // 0 থেকে 49 পর্যন্ত একটি যাদুঘর পূর্ণসংখ্যা উৎপন্ন করে
        double randDouble = rand.nextDouble(); // 0.0 থেকে 1.0 পর্যন্ত একটি যাদুঘর দ্বৈত সংখ্যা উৎপন্ন করে
        boolean randBoolean = rand.nextBoolean(); // একটি যাদুঘর বুলিয়ান উৎপন্ন করে
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
        System.out.println("Random Boolean: " + randBoolean);
    }
}
```

### `ThreadLocalRandom` ক্লাস ব্যবহার করা
যৌথ অপারেশনের জন্য, `ThreadLocalRandom` হল `Random` থেকে আরও দক্ষ।

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // 1 থেকে 100 পর্যন্ত
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // 1.0 থেকে 10.0 পর্যন্ত
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

### `SecureRandom` ক্লাস ব্যবহার করা
ক্রিপ্টোগ্রাফিক অপারেশনের জন্য, `SecureRandom` আরও উচ্চমাত্রার নিরাপত্তা প্রস্তাব করে।

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // bytes-কে নিরাপদ যাদুঘর সংখ্যা দিয়ে পূর্ণ করে 
        
        System.out.println("Secure Random Bytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## গভীর ডুব

কম্পিউটিংয়ের প্রাথমিক দিনগুলিতে যাদুঘর সংখ্যা নির্মাণ করার প্রক্রিয়া যথেষ্ট উন্নতি লাভ করে। Java-র `Random` ক্লাস প্রস্তাবনা প্রাথমিক প্রক্রিয়া ব্যবহার করে যাদুঘর সংখ্যা উৎপন্ন করে, যা নির্ধারণীয় এবং উচ্চ-নিরাপত্তা অ্যাপ্লিকেশনের জন্য উপযুক্ত নয়। এই কারণে, `SecureRandom` চালু করা হয়, যা আরও জটিল অ্যালগরিদমগুলি (যেমন SHA1PRNG) ব্যবহার করে ক্রিপ্টোগ্রাফিকভাবে শক্তিশালী যাদুঘর সংখ্যা উৎপন্ন করে।

যাইহোক, `Random` এবং `SecureRandom`-এর ত্রুটিগুলি আছে, যেমন মাল্টিথ্রেড পরিবেশে পারফরম্যান্সের হ্রাস। Java 7-এ `ThreadLocalRandom` ক্লাস চালু করা হয়েছিল এই সমস্যাটির সমাধানের জন্য, যা সুতা-স্থানীয় যাদুঘর সংখ্যা জেনারেটর প্রদান করে, যৌথ অ্যাপ্লিকেশনগুলিতে পারফরম্যান্স যথেষ্ট উন্নতি করে।

যদিও এই ক্লাসগুলি বেশিরভাগ চাহিদা পূরণ করে, অত্যন্ত উচ্চ-মাপের বা বিশেষায়িত প্রয়োজনের জন্য, ডেভেলপাররা অতিরিক্ত লাইব্রেরিগুলি অন্বেষণ করতে পারে বা কাস্টম সমাধান উন্নত করতে পারে। কৌশলটি নির্বাচন করা গুরুত্বপূর্ণ যা ব্যবহারের শর্তের নিরাপত্তা চাহিদা এবং পারফরম্যান্সের প্রয়োজনীয়তার উপর ভিত্তি করে।
