---
date: 2024-01-20 17:54:07.871672-07:00
description: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\
  \u0922\u093C\u0928\u093E \u090F\u0915 \u0910\u0938\u0940 \u092A\u094D\u0930\u0915\
  \u094D\u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947 \u0939\
  \u092E \u092B\u093C\u093E\u0907\u0932 \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\
  \u0930\u0940 \u0915\u094B \u092A\u0922\u093C \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0949\u0928\u094D\
  \u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928, \u0921\u0947\u091F\u093E \u0906\
  \u0926\u093E\u0928-\u092A\u094D\u0930\u0926\u093E\u0928, \u0932\u0949\u0917 \u092B\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.362122-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\
  \u093C\u0928\u093E \u090F\u0915 \u0910\u0938\u0940 \u092A\u094D\u0930\u0915\u094D\
  \u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947 \u0939\u092E\
  \ \u092B\u093C\u093E\u0907\u0932 \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\
  \u0940 \u0915\u094B \u092A\u0922\u093C \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0949\u0928\u094D\
  \u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928, \u0921\u0947\u091F\u093E \u0906\
  \u0926\u093E\u0928-\u092A\u094D\u0930\u0926\u093E\u0928, \u0932\u0949\u0917 \u092B\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u093E \u0935\u093F\u0936\u094D\u0932\u0947\
  \u0937\u0923 \u0935 \u0905\u0928\u094D\u092F \u0915\u093E\u092E\u094B\u0902 \u0915\
  \u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
