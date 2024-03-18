---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:02.725559-06:00
description: "YAML, \u09AF\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 YAML Ain't Markup\
  \ Language, \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\
  \u09CD\u09AF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \u0964 \u098F\u09B0 \u09B8\u09B9\u099C\u09B2\u09AD\u09CD\u09AF\u09A4\u09BE \u098F\
  \u09AC\u0982 \u09AA\u09BE\u09A0\u09CD\u09AF\u09A4\u09BE \u09B9\u09B2 XML \u0985\u09A5\
  \u09AC\u09BE JSON \u098F\u09B0 \u09AE\u09A4 \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09AF \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.058921-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 YAML Ain't Markup Language,\
  \ \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\
  \u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u0964\
  \ \u098F\u09B0 \u09B8\u09B9\u099C\u09B2\u09AD\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\
  \u0982 \u09AA\u09BE\u09A0\u09CD\u09AF\u09A4\u09BE \u09B9\u09B2 XML \u0985\u09A5\u09AC\
  \u09BE JSON \u098F\u09B0 \u09AE\u09A4 \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09AF \u09A1\u09C7\u099F\u09BE\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
YAML, যার অর্থ YAML Ain't Markup Language, একটি মানব-পাঠ্য ডেটা সিরিয়ালাইজেশন ফরম্যাট। এর সহজলভ্যতা এবং পাঠ্যতা হল XML অথবা JSON এর মত অন্যান্য ডেটা ফরম্যাটের তুলনায় অনেক সরল, যার কারণে প্রোগ্রামাররা প্রায়শই কনফিগারেশন ফাইল, ইন্টার-প্রসেস মেসেজিং, এবং ডেটা স্টোরেজে YAML ব্যবহার করে থাকেন।

## কিভাবে:
C# ভাষায় ডিফল্টভাবে YAML এর জন্য কোন সাপোর্ট নেই, কিন্তু *YamlDotNet* এর মত তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করে সহজেই YAML নিয়ে কাজ করা যায়। প্রথমে, আপনাকে YamlDotNet প্যাকেজ ইনস্টল করতে হবে:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### YAML পড়া:
কল্পনা করুন, আপনার কাছে নিম্নলিখিত কন্টেন্ট সহ একটি YAML ফাইল `config.yaml` আছে:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

আপনি C# এর মাধ্যমে এই YAML ফাইলটি পড়ে এবং পার্স করতে পারেন এরকম:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // নামিং কনভেনশন সংগঠন অনুযায়ী সামঞ্জস্য করুন
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**নমুনা আউটপুট:**
```
Name: MyApp, Version: 1.0.0
```

### YAML লেখা:
YAML ফাইলে ডেটা লেখার জন্য, YamlDotNet থেকে `Serializer` ক্লাস ব্যবহার করুন। এখানে কিভাবে একটি অবজেক্টকে YAML এ ফিরে সিরিয়ালাইজ করা হয়:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // নামিং কনভেনশন সংগঠন অনুযায়ী সামঞ্জস্য করুন
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**নমুনা আউটপুট:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

এই সরাসরি পদ্ধতি দেখায় কিভাবে আপনার C# প্রকল্পে YAML এর সাথে সম্পূর্ণ কার্যকর ভাবে কাজ করা যায়, YamlDotNet লাইব্রেরিকে ব্যবহার করে YAML ফাইল থেকে পড়া এবং তাতে লেখা সহজ করে তোলে।
