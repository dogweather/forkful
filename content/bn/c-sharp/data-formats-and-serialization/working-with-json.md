---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:13.676880-06:00
description: "JSON (JavaScript Object Notation) \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 JSON \u09A1\
  \u09C7\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8, \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F \u098F\u09AC\u0982 \u0995\u09CB\u09AF\u09BC\u09C7\u09B0\u09BF \u0995\
  \u09B0\u09BE, \u09AF\u09BE \u0986\u09A7\u09C1\u09A8\u09BF\u0995 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u0995\u099F\u09BF \u099C\u09B0\u09C1\u09B0\u09BF \u09A6\u0995\u09CD\
  \u09B7\u09A4\u09BE\u0964 \u098F\u0987\u2026"
lastmod: '2024-03-17T18:47:44.059889-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 JSON \u09A1\u09C7\
  \u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8, \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\
  \u099F \u098F\u09AC\u0982 \u0995\u09CB\u09AF\u09BC\u09C7\u09B0\u09BF \u0995\u09B0\
  \u09BE, \u09AF\u09BE \u0986\u09A7\u09C1\u09A8\u09BF\u0995 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u099C\u09B0\u09C1\u09B0\u09BF \u09A6\u0995\u09CD\u09B7\
  \u09A4\u09BE\u0964 \u098F\u0987 \u09A1\u09C7\u099F\u09BE-\u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u099A\u09C7\u099E\u09CD\u099C \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\u09BE \u098F\u09AC\
  \u0982 APIs \u098F \u0985\u09A4\u09CD\u09AF\u09A7\u09BF\u0995 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC \u0995\u09BE\u09B0\u09A3 \u098F\u099F\
  \u09BF\u09B0 \u09B8\u09B9\u099C \u09AA\u09A0\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE \u09B8\u09CD\u09AC\u09BE\u09A7\u09C0\
  \u09A8\u09A4\u09BE, \u09AF\u09BE \u09A8\u09C7\u099F\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09CD\u0995\u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09AD\u09BF\u09A4\u09CD\
  \u09A4\u09BF\u0995 \u09A1\u09C7\u099F\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F \u0995\u09B0\u09BE C# \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09A6\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF \u0995\
  \u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:


### JSON String থেকে Object এ পার্সিং
C# এ `System.Text.Json` নেমস্পেস প্রদান করে JSON প্রসেসিং এর জন্য। JSON string থেকে C# object এ পার্স করার জন্য, JSON কাঠামোর সাথে মেলে এমন একটি ক্লাস নির্ধারণ করুন এবং `JsonSerializer.Deserialize` মেথড ব্যবহার করুন।

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Name: John, Age: 30
    }
}
```

### একটি Object থেকে JSON জেনারেট করা
একটি C# অবজেক্ট থেকে আবার একটি JSON স্ট্রিং এ কনভারট করার জন্য, `JsonSerializer.Serialize` মেথড ব্যবহার করুন।

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Output: {"Name":"Jane","Age":25}
    }
}
```

### Newtonsoft.Json ব্যবহার করা
`Newtonsoft.Json` (অথবা Json.NET) একটি জনপ্রিয় তৃতীয় পক্ষের লাইব্রেরি যা JSON সিরিয়ালাইজেশন এবং ডিসিরিয়ালাইজেশন এর জন্য আরো বেশি লচকতা ও বিকল্প সরবরাহ করে।

Json.NET ব্যবহার করতে, প্রথমে আপনাকে NuGet এর মাধ্যমে `Newtonsoft.Json` প্যাকেজ ইনস্টল করতে হবে। তারপর, আপনি নিম্নরূপে JSON স্ট্রিং ডিসিরিয়ালাইজ করতে পারেন:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Name: Mike, Age: 22
    }
}
```

অবজেক্ট থেকে JSON জেনারেট করার জন্য Json.NET এর সাহায্যে:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Output: {"Name":"Ella","Age":28}
    }
}
```

এই স্নিপেটগুলি C# এ JSON নিয়ে কাজ শুরু করার জন্য একটি দ্রুত পথপ্রদর্শক, যেখানে `System.Text.Json` এর অন্তর্নির্মিত সামর্থ্য এবং `Newtonsoft.Json` এর ব্যাপক বৈশিষ্ট্য উভয়ই প্রদর্শিত হয়েছে।
