---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:27:39.793148-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
CSV (Comma-Separated Values) ফাইলগুলি একটি সাধারণ ডেটা বিনিময় ফরম্যাট যা সারণীবদ্ধ ডেটাকে সাদা টেক্সটে দেখায়, ব্যক্তিগত মানগুলি আলাদা করতে কমা ব্যবহার করে। প্রোগ্রামাররা বিভিন্ন অ্যাপ্লিকেশন এবং সেবাগুলির মধ্যে ডেটা আমদানি করতে, রপ্তানি করতে এবং ম্যানিপুলেট করতে CSV ফাইলগুলির সাথে কাজ করে, কারণ এটি একটি সাধারণ, ব্যাপকভাবে সমর্থিত ফরম্যাট যা স্প্রেডশিট অ্যাপ্লিকেশন, ডাটাবেস এবং প্রোগ্রামিং ভাষাগুলির সাথে সামঞ্জস্যপূর্ণ।

## কীভাবে:
C#-এ CSV ফাইলগুলি নিয়ে কাজ করা `System.IO` নামস্থান ব্যবহার করে মৌলিক অপারেশনগুলির জন্য, এবং আরও জটিল ম্যানিপুলেশন বা বড় ফাইলগুলি নির্বিঘ্নে সম্পাদনের জন্য, কেউ `CsvHelper` এর মত তৃতীয়-পক্ষের লাইব্রেরিগুলি বিবেচনা করতে পারেন। নীচে দুটি পদ্ধতিতে CSV ফাইল থেকে পড়া এবং CSV ফাইলে লেখার উদাহরণ দেওয়া হয়েছে।

### System.IO ব্যবহার করে CSV ফাইল পড়া
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // CSV ফাইলের সমস্ত লাইন পড়া
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"প্রথম কলাম: {rowData[0]}, দ্বিতীয় কলাম: {rowData[1]}");
        }
    }
}
```

**নমুনা আউটপুট:**
```
প্রথম কলাম: নাম, দ্বিতীয় কলাম: বয়স
প্রথম কলাম: জন ডো, দ্বিতীয় কলাম: 30
```

### System.IO ব্যবহার করে CSV ফাইলে লেখা
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var lines = new List<string>
        {
            "নাম,বয়স",
            "জন ডো,30",
            "জেন স্মিথ,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV ফাইল লেখা হয়েছে।");
    }
}
```

**নমুনা আউটপুট:**
```
CSV ফাইল লেখা হয়েছে।
```

### CsvHelper ব্যবহার করে CSV পড়া
CsvHelper ব্যবহার করে, প্রথমে, NuGet Package Manager ব্যবহার করে আপনার প্রকল্পে `CsvHelper` প্যাকেজটি যোগ করুন।

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"প্রথম কলাম: {record.Name}, দ্বিতীয় কলাম: {record.Age}");
            }
        }
    }
}
```

**নমুনা আউটপুট:**
```
প্রথম কলাম: জন ডো, দ্বিতীয় কলাম: 30
প্রথম কলাম: জেন স্মিথ, দ্বিতীয় কলাম: 25
```

### CsvHelper ব্যবহার করে CSV লেখা
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { Name = "জন ডো", Age = 30 },
            new Person { Name = "জেন স্মিথ", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CsvHelper এর সাথে CSV ফাইল লেখা হয়েছে।");
    }
}
```

**নমুনা আউটপুট:**
```
CsvHelper এর সাথে CSV ফাইল লেখা হয়েছে।
```
