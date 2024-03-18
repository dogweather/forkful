---
title:                "ডিরেক্টরি আছে কিনা পরীক্ষা করা"
date:                  2024-03-17T17:45:57.039372-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
জাভায় একটি ডিরেক্টরি বিদ্যমান আছে কিনা তা যাচাই করা একটি মৌলিক কাজ, যা ফাইল সিস্টেম ডিরেক্টরির উপস্থিতি যাচাই করে, এতে পড়ার, লেখার অথবা এর অস্তিত্ব প্রয়োজন হওয়া যেকোনো কাজ সঞ্চালনের আগে। ফাইল সিস্টেমের সাথে যেসব প্রোগ্রাম ইন্টারঅ্যাক্ট করে তার কোনো ত্রুটি বা ব্যতিক্রম এড়াতে এটি খুব জরুরি, যা মসৃণ এক্সিকিউশন এবং ভালো ব্যবহারকারী অভিজ্ঞতা নিশ্চিত করে।

## কিভাবে:
জাভাতে, একটি ডিরেক্টরি বিদ্যমান আছে কিনা তা যাচাই করার জন্য প্রধানত `java.nio.file.Files` এবং `java.io.File` ক্লাসগুলি ব্যবহার করে বেশ কিছু উপায় রয়েছে।

**`java.nio.file.Files` ব্যবহার করে**:

সাম্প্রতিক জাভা সংস্করণগুলিতে এই পদ্ধতিটি প্রস্তাবিত।

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // এখানে ডিরেক্টরির পথ নির্দিষ্ট করুন
        String directoryPath = "path/to/directory";

        // ডিরেক্টরি বিদ্যমান আছে কিনা যাচাই করা হচ্ছে
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("ডিরেক্টরি বিদ্যমান আছে।");
        } else {
            System.out.println("ডিরেক্টরি বিদ্যমান নেই।");
        }
    }
}
```
**নমুনা আউটপুট**:
```
ডিরেক্টরি বিদ্যমান আছে।
```
অথবা 
```
ডিরেক্টরি বিদ্যমান নেই।
```

**`java.io.File` ব্যবহার করে**:

যদিও `java.nio.file.Files` প্রস্তাবিত, পুরানো `java.io.File` ক্লাসও ব্যবহৃত হতে পারে।

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // এখানে ডিরেক্টরির পথ নির্দিষ্ট করুন
        String directoryPath = "path/to/directory";

        // একটি ফাইল অবজেক্ট তৈরি করা
        File directory = new File(directoryPath);

        // ডিরেক্টরি বিদ্যমান আছে কিনা যাচাই করা হচ্ছে
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("ডিরেক্টরি বিদ্যমান আছে।");
        } else {
            System.out.println("ডিরেক্টরি বিদ্যমান নেই।");
        }
    }
}
```
**নমুনা আউটপুট**:
```
ডিরেক্টরি বিদ্যমান আছে।
```
অথবা
```
ডিরেক্টরি বিদ্যমান নেই।
```

**থার্ড-পার্টি লাইব্রেরি ব্যবহার করে**:

যদিও স্ট্যান্ডার্ড জাভা লাইব্রেরি প্রায়শই এই কাজের জন্য যথেষ্ট, আপাচি কমন্স IO এর মতো থার্ড-পার্টি লাইব্রেরিগুলি অতিরিক্ত ফাইল হ্যান্ডলিং ইউটিলিটিগুলি অফার করে যা আরও জটিল অ্যাপ্লিকেশনে উপকারী হতে পারে।

**আপাচি কমন্স IO**:

প্রথমে, আপনার প্রজেক্টে আপাচি কমন্স IO ডিপেনডেন্সি যোগ করুন। তারপর, আপনি এর বৈশিষ্ট্যগুলি ব্যবহার করে ডিরেক্টরির অস্তিত্ব যাচাই করতে পারেন।

```java
// ধরে নেওয়া যাক আপাচি কমন্স IO প্রজেক্টে যোগ করা হয়েছে

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // এখানে ডিরেক্টরির পথ নির্দিষ্ট করুন
        String directoryPath = "path/to/directory";

        // FileUtils ব্যবহার করে যাচাই করা হচ্ছে
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("ডিরেক্টরি বিদ্যমান আছে।");
        } else {
            System.out.println("ডিরেক্টরি বিদ্যমান নেই।");
        }
    }
}
```

**নোট**: `FileUtils.directoryContains` যদি একটি ডিরেক্টরি বিশেষ একটি ফাইল ধারণ করে এমনটা যাচাই করে, কিন্তু দ্বিতীয় যুক্তিতে `null` পাস করে, আপনি এটি ডিরেক্টরির অস্তিত্ব যাচাই করার জন্য ব্যবহার করতে পারেন। এটি সবচেয়ে সরাসরি বা উদ্দেশ্যমূলক ব্যবহার নাও হতে পারে বলে সচেতন থাকুন।

**নমুনা আউটপুট**:
```
ডিরেক্টরি বিদ্যমান আছে।
```
অথবা
```
ডিরেক্টরি বিদ্যমান নেই।
```
