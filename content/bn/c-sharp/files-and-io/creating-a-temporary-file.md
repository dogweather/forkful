---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:12.040775-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ C#-\u098F \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09A4\u09BE\
  \u09A4\u09C7 \u09B2\u09BF\u0996\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09A6\u09CD\
  \u09B0\u09C1\u09A4 \u0989\u09AA\u09BE\u09AF\u09BC \u0989\u09B2\u09CD\u09B2\u09C7\
  \u0996 \u0995\u09B0\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.057881-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 C#-\u098F \u098F\u0995\u099F\u09BF \u0985\
  \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\
  \u09B0\u09BF \u098F\u09AC\u0982 \u09A4\u09BE\u09A4\u09C7 \u09B2\u09BF\u0996\u09BE\
  \u09B0 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u0989\u09B2\u09CD\u09B2\u09C7\u0996 \u0995\u09B0\u09BE \u09B9\u09B2\
  ."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
এখানে C#-এ একটি অস্থায়ী ফাইল তৈরি এবং তাতে লিখার একটি দ্রুত উপায় উল্লেখ করা হল:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // একটি অস্থায়ী ফাইল তৈরি করুন
        string tempFilePath = Path.GetTempFileName();

        // অস্থায়ী ফাইলে কিছু লিখুন
        File.WriteAllText(tempFilePath, "Hello, Temp World!");

        // অস্থায়ী ফাইল থেকে পড়ুন এবং প্রিন্ট করুন
        string fileContents = File.ReadAllText(tempFilePath);
        Console.WriteLine(fileContents);

        // অস্থায়ী ফাইলটি পরিষ্কার করুন
        File.Delete(tempFilePath);
    }
}
```

নমুনা আউটপুট:
```
Hello, Temp World!
```

## গভীর ডুব
কম্পিউটিং-এর প্রাথমিক দিনগুলিতে মেমোরি ব্যবহার কমানো জরুরি ছিল যখন থেকেই অস্থায়ী ফাইলগুলি ব্যবহৃত হয়ে আসছে। এগুলি প্রোগ্রামগুলির জন্য ডেটা নিয়ে কাজ করার একটি নিরাপদ পরিবেশ প্রদান করে বিনা দীর্ঘমেয়াদী সংরক্ষণ ফলে।

`Path.GetTempFileName()` ছাড়াও, আপনার অন্যান্য বিকল্প আছে যেমন `Path.GetRandomFileName()`, যা কোনো ফাইল তৈরি না করে একটি নাম দেয় যা আপনি একটি অস্থায়ী ফাইলের জন্য ব্যবহার করতে পারেন। এছাড়া, `System.IO.TempFileCollection` শ্রেণী একাধিক অস্থায়ী ফাইল পরিচালনা করতে পারে, যা তখন সুবিধাজনক হয় যখন আপনার একাধিক ফাইলের প্রয়োজন হয়।

প্রায়শই, C# এর অস্থায়ী ফাইল তৈরির জন্য ব্যবহৃত পদ্ধতিগুলি তার নিম্নলিখিত অপারেটিং সিস্টেম দ্বারা প্রদত্ত APIs ব্যবহার করে। উইন্ডোজে, `GetTempFileName()` একটি তুলনীয় Win32 API ফাংশনের সাথে ম্যাপ করা হয়, যা ফাইলের নামের অনন্যতা নিশ্চিত করে এবং সংঘর্ষের বিরুদ্ধে এটি নিরাপদ করে।

সর্বদা অস্থায়ী ফাইলগুলি মুছে ফেলার কথা মনে রাখুন। যদিও এগুলি একটি temp ডিরেক্টরিতে থাকে, তবে এগুলি অবহেলা করলে জমা হতে থাকতে পারে, যা এক ধরনের ডিজিটাল আবর্জনার দুঃস্বপ্নে পরিণত হতে পারে।

## দেখুন এছাড়াও
আরও পড়া এবং গভীর বোঝার জন্য, এই লিঙ্কগুলি আপনার প্রয়োজনীয় সমস্ত কিছু সম্পর্কে আবরণ করে:

- ডটনেট-এ অস্থায়ী ফাইল সম্পর্কে মাইক্রোসফ্টের অফিসিয়াল ডকুমেন্টেশন:
  [.NET এ অস্থায়ী ফাইল](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-temporary-file)

- C#-এ ফাইল এবং স্ট্রিমগুলি নিয়ে কাজ করার সেরা অনুশীলন:
  [File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io)

- যদি আপনি ফাইল I/O নিরাপত্তা বিবেচনা অন্বেষণ করতে চান:
  [File I/O and Security](https://docs.microsoft.com/en-us/dotnet/standard/security/secure-file-i-o)
