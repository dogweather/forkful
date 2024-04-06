---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:18.329002-07:00
description: "\u0915\u0948\u0938\u0947: C# \u092E\u0947\u0902 CSV \u092B\u093C\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\
  \u092F \u0915\u0930\u0928\u093E `System.IO` \u0928\u0947\u092E\u0938\u094D\u092A\
  \u0947\u0938 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u092E\
  \u0942\u0932\u092D\u0942\u0924 \u0911\u092A\u0930\u0947\u0936\u0928\u094B\u0902\
  \ \u0915\u0947 \u0932\u093F\u090F, \u0914\u0930 \u0905\u0927\u093F\u0915 \u091C\u091F\
  \u093F\u0932 \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092F\u093E \u092C\u0921\u093C\u0940 \u092B\u093C\u093E\u0907\u0932\u094B\u0902\
  \ \u0915\u094B \u0938\u0939\u091C\u0924\u093E\u2026"
lastmod: '2024-04-05T21:53:54.364891-06:00'
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u093E\
  \ `System.IO` \u0928\u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u0915\u0947 \u092E\
  \u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u092E\u0942\u0932\u092D\u0942\u0924\
  \ \u0911\u092A\u0930\u0947\u0936\u0928\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F\
  , \u0914\u0930 \u0905\u0927\u093F\u0915 \u091C\u091F\u093F\u0932 \u0939\u0947\u0930\
  \u092B\u0947\u0930 \u0915\u0947 \u0932\u093F\u090F \u092F\u093E \u092C\u0921\u093C\
  \u0940 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u0938\u0939\u091C\
  \u0924\u093E \u0938\u0947 \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F, \u0915\u094B\u0908 `CsvHelper` \u091C\u0948\u0938\u0947 \u0924\
  \u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\
  \u0915\u093E\u0932\u092F\u094B\u0902 \u092A\u0930 \u0935\u093F\u091A\u093E\u0930\
  \ \u0915\u0930 \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\u091A\u0947\
  \ \u0926\u094B\u0928\u094B\u0902 \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\
  \u0923\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u092A\u0922\u093C\
  \u0928\u0947 \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\
  \u0947 \u0915\u0947 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F \u0917\
  \u090F \u0939\u0948\u0902\u0964."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

## कैसे:
C# में CSV फ़ाइलों के साथ कार्य करना `System.IO` नेमस्पेस के माध्यम से मूलभूत ऑपरेशनों के लिए, और अधिक जटिल हेरफेर के लिए या बड़ी फ़ाइलों को सहजता से संभालने के लिए, कोई `CsvHelper` जैसे तृतीय-पक्ष पुस्तकालयों पर विचार कर सकता है। नीचे दोनों दृष्टिकोणों का उपयोग करके CSV फ़ाइलों से पढ़ने और उनमें लिखने के उदाहरण दिए गए हैं।

### System.IO का उपयोग करके CSV फ़ाइल पढ़ना
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // CSV फ़ाइल की सभी लाइनों को पढ़ना
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"पहला कॉलम: {rowData[0]}, दूसरा कॉलम: {rowData[1]}");
        }
    }
}
```

**नमूना आउटपुट:**
```
पहला कॉलम: नाम, दूसरा कॉलम: उम्र
पहला कॉलम: जॉन डो, दूसरा कॉलम: 30
```

### System.IO का उपयोग करके CSV फ़ाइल में लिखना
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
            "नाम,उम्र",
            "जॉन डो,30",
            "जेन स्मिथ,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV फ़ाइल लिखी गई।");
    }
}
```

**नमूना आउटपुट:**
```
CSV फ़ाइल लिखी गई।
```

### CsvHelper का उपयोग करके CSV पढ़ना
CsvHelper का उपयोग करने के लिए, सबसे पहले अपने प्रोजेक्ट में NuGet पैकेज मैनेजर का उपयोग करके `CsvHelper` पैकेज जोड़ें।

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
                Console.WriteLine($"पहला कॉलम: {record.Name}, दूसरा कॉलम: {record.Age}");
            }
        }
    }
}
```

**नमूना आउटपुट:**
```
पहला कॉलम: जॉन डो, दूसरा कॉलम: 30
पहला कॉलम: जेन स्मिथ, दूसरा कॉलम: 25
```

### CsvHelper का उपयोग करके CSV लिखना
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
        उम्र { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { Name = "जॉन डो", Age = 30 },
            new Person { Name = "जेन स्मिथ", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CsvHelper के साथ CSV फ़ाइल लिखी गई।");
    }
}
```

**नमूना आउटपुट:**
```
CsvHelper के साथ CSV फ़ाइल लिखी गई।
```
