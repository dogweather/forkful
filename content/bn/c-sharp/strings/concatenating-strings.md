---
title:                "স্ট্রিং জোড়া দেওয়া"
date:                  2024-03-17T17:46:34.658373-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

স্ট্রিং কনক্যাটিনেশন হলো স্ট্রিংগুলি শেষ-থেকে-শেষ জুড়ে দেওয়ার প্রক্রিয়া। আমরা এটা করি কারণ প্রায়শই আমাদেরকে শব্দ অথবা প্রতীকসমূহ জুড়ে বাক্য, বার্তা তৈরি করতে হয় অথবা পাঠ্য হিসেবে গণনাযোগ্য মান তৈরি করতে হয়।

## কিভাবে:

C#-এ স্ট্রিং কনক্যাটিনেশন কয়েকটি উপায়ে করা যায়:

`+` অপারেটর ব্যবহার করে:
```C#
string hello = "Hello";
string world = "World";
string concatenated = hello + ", " + world + "!";
Console.WriteLine(concatenated); // আউটপুট: Hello, World!
```

`String.Concat()` মেথড ব্যবহার করে:
```C#
string concatenated = String.Concat("Hello", ", ", "World", "!");
Console.WriteLine(concatenated); // আউটপুট: Hello, World!
```

লুপে দক্ষতার জন্য `StringBuilder` ব্যবহার করে:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(", ");
sb.Append("World");
sb.Append("!");
Console.WriteLine(sb.ToString()); // আউটপুট: Hello, World!
```

স্ট্রিং ইন্টারপোলেশন ব্যবহার করে (C# 6.0 এবং তার উপরে):
```C#
string world = "World";
string concatenated = $"Hello, {world}!";
Console.WriteLine(concatenated); // আউটপুট: Hello, World!
```

## গভীর ডাইভ

স্ট্রিং কনক্যাটিনেশন নতুন কিছু নয়; এটা প্রোগ্রামিংয়ের প্রারম্ভিক দিন থেকেই আছে। যাইহোক, C#-এ এটা করার উপায় বিবর্তিত হয়েছে। মূলত, `+` বিস্তর ব্যবহৃত হতো, কিন্তু লুপের মধ্যে এটা সবসময় দক্ষ নয়, কারণ .NET-এ স্ট্রিংস পরিবর্তনীয় নয়। প্রতিটি `+` অপারেশন একটি নতুন স্ট্রিং তৈরি করে, যা কর্মক্ষমতা সম্পর্কিত সমস্যা ঘটাতে পারে।

`String.Concat()` একটি সরাসরি মেথড কল যা লুপ-বান্ধব নয় কিন্তু কম, পরিচিত সংখ্যক স্ট্রিং জন্য ঠিক আছে।

লুপ পরিস্থিতিতে অথবা একটি স্ট্রিং ক্রমিক ভাবে গঠন করতে `StringBuilder` হলো যাওয়ার পথ। অন্তরালে, `StringBuilder` একটি বাফার বজায় রাখে যা প্রতিটি যোগ অপারেশনের জন্য নতুন স্ট্রিং তৈরি না করে অতিরিক্তগুলি সামলানোর ক্ষমতা রাখে।

স্ট্রিং ইন্টারপোলেশন, C# 6.0-এ প্রবর্তিত, আরও পড়তে সহজ এবং রক্ষণাবেক্ষণের জন্য অনুকূল কোড তৈরি করে। এটি কম্পাইল টাইমে `String.Format()` কলে রূপান্তারিত হয় তবে চোখের জন্যে সহজ এবং ভুলের প্রবণতা কম।

প্রতিটি পদ্ধতির নিজস্ব স্থান আছে: দ্রুত কনক্যাটিনেশন (`+`), কয়েকটি স্ট্রিং জোড়া (`String.Concat()`), ভারী-ডিউটি স্ট্রিং বিল্ডিং (`StringBuilder`), এবং পরিষ্কার, ফরম্যাটেড স্ট্রিংস (স্ট্রিং ইন্টারপোলেশন)।

## আরও দেখুন

- স্ট্রিং কনক্যাটিনেশন সম্পর্কে Microsoft Docs: [String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- `StringBuilder` সম্পর্কে Microsoft Docs: [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
