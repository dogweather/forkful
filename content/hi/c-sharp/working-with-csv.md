---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV का मतलब है Comma-Separated Values, यानी ऐसी फाइल जिसमें डेटा अल्पविराम से अलग होता है। प्रोग्रामर्स आमतौर पर डेटा एक्सचेंज, बैकअप, डेटा आयात-निर्यात के लिए CSV का इस्तेमाल करते हैं क्योंकि यह सामान्य, पठनीय और सरल प्रारूप होता है।

## How to: (कैसे करें?)
```C#
using System;
using System.Collections.Generic;
using System.IO;

class CsvExample
{
    static void Main()
    {
        string csvPath = "data.csv";

        // CSV फाइल पढ़ना
        List<string[]> rows = ReadCsvFile(csvPath);
        
        // पढ़े गए डेटा को कंसोल पर प्रिंट करना
        foreach (var row in rows)
        {
            Console.WriteLine(string.Join(", ", row));
        }

        // CSV फाइल लिखना
        string[] newRow = { "4", "नया उपयोगकर्ता", "ny@domain.com" };
        WriteToCsvFile(csvPath, newRow);
    }

    static List<string[]> ReadCsvFile(string csvPath)
    {
        var rows = new List<string[]>();
        using (var reader = new StreamReader(csvPath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] row = line.Split(',');
                rows.Add(row);
            }
        }
        return rows;
    }

    static void WriteToCsvFile(string csvPath, string[] newRow)
    {
        using (var writer = new StreamWriter(csvPath, true))
        {
            string line = string.Join(",", newRow);
            writer.WriteLine(line);
        }
    }
}
```
**Output:**
```
1, सुरेश, suresh@example.com
2, अमित, amit@domain.com
3, प्रिया, priya@site.in
4, नया उपयोगकर्ता, ny@domain.com
```

## Deep Dive (गहराई से जानकारी)
CSV फॉर्मेट 1970 के दशक में hi लोकप्रिय हुआ था। आज भी इसका इस्तेमाल व्यापक होता है। XML और JSON जैसे फॉर्मेट alternative होते हैं, पर CSV अपनी सरलता के लिए पसंद की जाती है। C# में CSV फाइल्स से निपटने के लिए `StreamReader` और `StreamWriter` क्लासेस का उपयोग किया जाता है, पर third-party लाइब्रेरी जैसे `CsvHelper` से काम और भी सरल हो जाता है।

## See Also (और जानकारी)
- Microsoft CSV documentation: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- `CsvHelper` library: [CsvHelper](https://joshclose.github.io/CsvHelper/)
- CSV पर अधिक जानकारी के लिए RFC 4180 देखें: [tools.ietf.org](https://tools.ietf.org/html/rfc4180)
