---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases: - /hi/c-sharp/reading-a-text-file.md
date:                  2024-01-20T17:54:07.871672-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
पाठ फ़ाइल को पढ़ना एक ऐसी प्रक्रिया है जिससे हम फ़ाइल की सामग्री को पढ़ सकते हैं। प्रोग्रामर्स इसका उपयोग कॉन्फ़िगरेशन, डेटा आदान-प्रदान, लॉग फाइलों का विश्लेषण व अन्य कामों के लिए करते हैं।

## कैसे करें? (How to:)
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = "example.txt"; // फ़ाइल पथ को यहाँ प्रदान करें

        // फ़ाइल पढ़ने का सबसे सरल तरीका
        string content = File.ReadAllText(filePath);
        Console.WriteLine(content);

        // लाइन दर लाइन फ़ाइल पढ़ना
        string[] lines = File.ReadAllLines(filePath);
        foreach (string line in lines)
        {
            Console.WriteLine(line);
        }
        
        // फ़ाइल से स्ट्रीम के जरिये पढ़ना
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

सैंपल आउटपुट:
```
नमस्ते! यह एक उदाहरण पाठ फ़ाइल है।
यहाँ कुछ टेक्स्ट है।
```

## गहराई में (Deep Dive)
पाठ फ़ाइलों को पढ़ने का इतिहास बहुत पुराना है - आरंभिक प्रोग्रामिंग काल से ही इसका उपयोग होता आ रहा है। पाठ फ़ाइलों को पढ़ने के लिए `System.IO` नेमस्पेस में कई क्लासेस और मेथड्स उपलब्ध हैं। `File.ReadAllText()`, `File.ReadAllLines()` और `StreamReader` क्लास इनमें सबसे आम हैं। 

विकल्प के तौर पर, एसिंक्रोनस मेथड्स जैसे `ReadAllTextAsync()`, `ReadAllLinesAsync()`, और `StreamReader` का `ReadLineAsync()` मेथड उपयोग किये जा सकते हैं, जिससे बड़ी फ़ाइलों को पढ़ते समय एप्लिकेशन रेस्पोंसिव बना रहता है। 

कार्यान्वयन विवरणों की बात करें, तो सुरक्षा के दृष्टिकोण से `StreamReader` तब उपयोगी होता है जब फाइल का साइज अनिश्चित हो, इससे हमें मेमोरी मैनेजमेंट में सहायता मिलती है।

## और भी देखें (See Also)
- [StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-6.0)
- [File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-6.0)
- [Asynchronous File I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o)
