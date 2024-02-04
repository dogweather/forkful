---
title:                "CSV के साथ काम करना"
date:                  2024-02-03T19:20:18.329002-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV (कॉमा-सेपरेटेड वैल्यूज़) फ़ाइलें एक सामान्य डेटा एक्सचेंज प्रारूप हैं जो सादे पाठ में टेबुलर डेटा का प्रतिनिधित्व करती हैं, प्रत्येक मान को अलग करने के लिए अल्पविराम का उपयोग करती हैं। प्रोग्रामर्स विभिन्न एप्लिकेशन्स और सेवाओं में डेटा को आयात करने, निर्यात करने और उसे संभालने के लिए CSV फ़ाइलों के साथ कार्य करते हैं, क्योंकि यह एक सरल, व्यापक रूप से समर्थित प्रारूप है जो स्प्रेडशीट एप्लिकेशन्स, डेटाबेस, और प्रोग्रामिंग भाषाओं के साथ संगत है।

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
