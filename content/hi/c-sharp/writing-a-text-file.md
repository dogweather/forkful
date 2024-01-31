---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल लिखना डेटा को साधारण पाठ रूप में स्टोर करने की प्रक्रिया है। प्रोग्रामर्स इसे लॉग्स, सेटिंग्स, संदेश, या डेटा एक्सचेंज के लिए करते हैं।

## कैसे करें:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\test.txt";
        string textToWrite = "नमस्ते, यह एक उदाहरण टेक्स्ट है।";

        // टेक्स्ट फाइल बनाना और लिखना
        File.WriteAllText(filePath, textToWrite);

        // जाँच करें कि टेक्स्ट लिखा गया है
        if (File.Exists(filePath))
        {
            Console.WriteLine("फाइल सफलतापूर्वक लिखी गई!");
        }
    }
}
```

सैंपल आउटपुट:

```
फाइल सफलतापूर्वक लिखी गई!
```

## गहराई से जानकारी:

पाठ फाइलों को लिखने का विधि पहली बार में कमांड लाइन और बैच प्रोसेसिंग सिस्टम्स में उपयोगी थी। अन्य विकल्पों में XML, JSON, या बाइनरी फाइलें शामिल हैं, परन्तु सरलता और पठनीयता के लिए टेक्स्ट फाइलें अभी भी लोकप्रिय हैं। `StreamWriter` और `FileStream` जैसी कक्षाएं अधिक सूक्ष्म नियंत्रण के लिए इस्तेमाल की जाती हैं।

## देखें भी:

- Microsoft का डॉक्युमेंटेशन ऑन `System.IO` namespace: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- ट्यूटोरियल वेबसाइट `File Handling in C#`: [Tutorialspoint](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
