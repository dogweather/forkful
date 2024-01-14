---
title:                "C#: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV फ़ाइलें सामान्यतः तार के रूप में उपयोग की जाती हैं और डेटा को सही ढंग से फॉर्मैट करने के लिए उपयोगी होती हैं। इनका उपयोग डेटा विश्लेषण, रिपोर्टिंग और विभिन्न ऐप्स में डेटा को भेजने के लिए किया जाता है।

## कैसे करें

```C#
// सैम्पल CSV फ़ाइल खोलें 
using (var reader = new StreamReader("sample.csv"))
{
    // CSV फ़ाइल को रो प्रकार में पढ़ें
    while (!reader.EndOfStream)
    {
        var line = reader.ReadLine();
        // यदि लाइन मैच करता है, तो उसे स्प्लिट करें
        if (line.Contains("John"))
        {
            var values = line.Split(',');
            // स्प्लिट किए गए वैल्यूज को कॉन्सोल पर प्रिंट करें
            Console.WriteLine(values[0] + " " + values[1]);
        }
    }
}
```
आउटपुट:
```
John Doe
John Smith
```

## गहराई में

CSV फ़ाइलों को प्रोग्रामिंग भाषाओं में खोलने के लिए, प्रक्रिया गणना के दौरान एक या अधिक कॉमा द्वारा अलग किए गए पदों को भी कॉमा डाटा के रूप में जाना जाता है। यह एक आसान तरीका है डेटा संरचना को सुधारने का और उसे अन्य ऐप्स में स्थानांतरित करने का।

## और भी देखें

- [CSV फ़ाइलों को C# में खोलना](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time)
- [C# संस्करण 7.3 के साथ CSVHelper पुस्तकालय का उपयोग करना](https://www.nuget.org/packages/CsvHelper/7.3.1)
- [CSV फ़ाइल का विश्‍लेषण और पास्त्रावली करना](https://www.techonthenet.com/excel/formulas/csv.php)