---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "C#: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

एक अस्थायी फ़ाइल बनाना, सामान्यतया समयीकारी डाटा स्थान रखने का एक आम तरीका है। इसका उपयोग अनुप्रयोगों के दौरान अस्थायी डेटा को स्थानांतरित करने या साफ़ करने के लिए किया जाता है।

# कैसे करें?

```C#
var tempFile = Path.GetTempFileName();
```

यह कोड अस्थायी फाइल का नाम और उसकी पथ जेनेरेट करता है जिसे प्रोग्रामर इस्तेमाल कर सकते हैं।

```
C:\Users\YourUsername\AppData\Local\Temp\0g4zh4an.vtg
```

इस अस्थायी फाइल में दिए गए नाम और पथ को आप अपने इच्छानुसार बदल सकते हैं।

# गहराई पर जाएं

पहले से ही कई विकल्प मौजूद हैं जो अस्थायी फाइल बनाने के लिए उपयोग किए जा सकते हैं। इनमे से कुछ विकल्प अस्थायी फाइल के लिए खुद ही स्थान देते हैं जबकि कुछ आपको स्थान खुद निर्देशित करने के लिए स्वतंत्रता देते हैं।

## उदाहरण


```C#
using System;
using System.IO;

namespace TempFiles
{
    class Program
    {
        static void Main(string[] args)
        {
            string tempPath = Path.GetTempFileName();

            Console.WriteLine("Temporary file created at " + tempPath);

            File.WriteAllText(tempPath, "This is a temporary file");

            Console.WriteLine("Contents of temporary file:");

            string content = File.ReadAllText(tempPath);

            Console.WriteLine(content);

            File.Delete(tempPath);

            Console.WriteLine("Temporary file deleted.");
        }
    }
}
```

उपरोक्त कोड अस्थायी फाइल बनाने, उसमे कुछ लिखने और फिर उसे हटाने का एक उदाहरण है।

# और भी देखें

आप अस्थायी फाइलों के सम्बन्ध में और जानकारी के लिए Microsoft की [आधिकारिक वेबसाइट](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename) पर जा सकते हैं।