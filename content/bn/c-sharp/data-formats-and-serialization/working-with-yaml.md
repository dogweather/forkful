---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:02.725559-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C# \u09AD\u09BE\u09B7\u09BE\u09DF\
  \ \u09A1\u09BF\u09AB\u09B2\u09CD\u099F\u09AD\u09BE\u09AC\u09C7 YAML \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\
  \u099F \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 *YamlDotNet* \u098F\
  \u09B0 \u09AE\u09A4 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\u099C\u09C7\u0987\
  \ YAML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\
  \u09BE\u09DF\u0964\u2026"
lastmod: '2024-03-17T18:47:44.058921-06:00'
model: gpt-4-0125-preview
summary: "C# \u09AD\u09BE\u09B7\u09BE\u09DF \u09A1\u09BF\u09AB\u09B2\u09CD\u099F\u09AD\
  \u09BE\u09AC\u09C7 YAML \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\
  \ \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\
  \u09CD\u09A4\u09C1 *YamlDotNet* \u098F\u09B0 \u09AE\u09A4 \u09A4\u09C3\u09A4\u09C0\
  \u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09B8\u09B9\u099C\u09C7\u0987 YAML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09BE\u09DF\u0964 \u09AA\u09CD\u09B0\u09A5\
  \u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 YamlDotNet \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09AC\u09C7."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
