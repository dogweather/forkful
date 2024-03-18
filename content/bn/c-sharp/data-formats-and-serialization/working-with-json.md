---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:13.676880-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

JSON (JavaScript Object Notation) নিয়ে কাজ করা মানে হল JSON ডেটা পার্স, জেনারেট এবং কোয়েরি করা, যা আধুনিক প্রোগ্রামিং এর জন্য একটি জরুরি দক্ষতা। এই ডেটা-ইন্টারচেঞ্জ ফরম্যাটটি ওয়েব সেবা এবং APIs এ অত্যধিক ব্যবহৃত হয় কারণ এটির সহজ পঠনীয়তা এবং ভাষা স্বাধীনতা, যা নেটওয়ার্কভিত্তিক অ্যাপ্লিকেশন কাজ করা অথবা ওয়েব ভিত্তিক ডেটার সাথে ইন্টারেক্ট করা C# প্রোগ্রামারদের জন্য অপরিহার্য করে তোলে।

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
