---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:46.484469-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:44.052500-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:


### System.IO ব্যবহার করে
C# `System.IO` নামস্থান প্রদান করে যা `Directory` ক্লাস ধারণ করে, যা একটি ডিরেক্টরির অস্তিত্ব `Exists` পদ্ধতির মাধ্যমে সরাসরি যাচাই করার একটি সরাসরি উপায় অফার করে।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // যাচাই করুন যে ডিরেক্টরি অস্তিত্ব আছে কি না
        bool directoryExists = Directory.Exists(directoryPath);

        // ফলাফল প্রিন্ট করুন
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**নমুনা আউটপুট:**

```
Directory exists: False
```

যদি ডিরেক্টরি `C:\ExampleDirectory` পথে অস্তিত্বাবে থাকে, তাহলে আউটপুট হবে `True`.

### ইউনিট টেস্টিংয়ের জন্য System.IO.Abstractions ব্যবহার করে
আপনার কোডকে ইউনিট টেস্টেবল করে তোলা, বিশেষত যখন এটি ফাইল সিস্টেমের সঙ্গে ইন্টারঅ্যাক্ট করে, `System.IO.Abstractions` প্যাকেজটি একটি জনপ্রিয় বিকল্প। এটি আপনাকে আপনার টেস্টগুলিতে ফাইল সিস্টেম অপারেশনগুলি অভস্ট্রাক্ট ও মক করতে অনুমতি দেয়। এখানে আপনি একটি ডিরেক্টরির অস্তিত্ব যাচাই করার উপায়:

প্রথমে, নিশ্চিত করুন আপনি প্যাকেজটি ইনস্টল করেছেন:

```
Install-Package System.IO.Abstractions
```

তারপর, আপনি আপনার ক্লাসে একটি `IFileSystem` ইনজেক্ট করতে পারেন এবং এটি ব্যবহার করে একটি ডিরেক্টরির অস্তিত্ব যাচাই করতে পারেন, যা ইউনিট টেস্টিংকে আরও সহজ করে।

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**নমুনা আউটপুট:**

```
Directory exists: False
```

এই পদ্ধতি আপনার অ্যাপ্লিকেশন লজিককে সরাসরি ফাইল সিস্টেম অ্যাক্সেস থেকে ডেকাপল করে, যা আপনার কোডকে আরও মডুলার, টেস্টেবল এবং মেনটেইনেবল করে তোলে।
