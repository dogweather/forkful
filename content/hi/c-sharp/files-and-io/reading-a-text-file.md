---
date: 2024-01-20 17:54:07.871672-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u092A\u093E\
  \u0920 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u092A\u0922\u093C\
  \u0928\u0947 \u0915\u093E \u0907\u0924\u093F\u0939\u093E\u0938 \u092C\u0939\u0941\
  \u0924 \u092A\u0941\u0930\u093E\u0928\u093E \u0939\u0948 - \u0906\u0930\u0902\u092D\
  \u093F\u0915 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u0915\u093E\u0932 \u0938\u0947 \u0939\u0940 \u0907\u0938\u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0906 \u0930\u0939\u093E \u0939\u0948\
  \u0964 \u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B\
  \ \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-04-05T22:51:07.048383-06:00'
model: gpt-4-1106-preview
summary: ") \u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B\
  \ \u092A\u0922\u093C\u0928\u0947 \u0915\u093E \u0907\u0924\u093F\u0939\u093E\u0938\
  \ \u092C\u0939\u0941\u0924 \u092A\u0941\u0930\u093E\u0928\u093E \u0939\u0948 - \u0906\
  \u0930\u0902\u092D\u093F\u0915 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u093F\u0902\u0917 \u0915\u093E\u0932 \u0938\u0947 \u0939\u0940 \u0907\u0938\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0906 \u0930\u0939\
  \u093E \u0939\u0948\u0964 \u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932\u094B\
  \u0902 \u0915\u094B \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `System.IO` \u0928\u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u092E\u0947\u0902\
  \ \u0915\u0908 \u0915\u094D\u0932\u093E\u0938\u0947\u0938 \u0914\u0930 \u092E\u0947\
  \u0925\u0921\u094D\u0938 \u0909\u092A\u0932\u092C\u094D\u0927 \u0939\u0948\u0902\
  \u0964 `File.ReadAllText()`, `File.ReadAllLines()` \u0914\u0930 `StreamReader` \u0915\
  \u094D\u0932\u093E\u0938 \u0907\u0928\u092E\u0947\u0902 \u0938\u092C\u0938\u0947\
  \ \u0906\u092E \u0939\u0948\u0902\u0964 \u0935\u093F\u0915\u0932\u094D\u092A \u0915\
  \u0947 \u0924\u094C\u0930 \u092A\u0930, \u090F\u0938\u093F\u0902\u0915\u094D\u0930\
  \u094B\u0928\u0938 \u092E\u0947\u0925\u0921\u094D\u0938 \u091C\u0948\u0938\u0947\
  \ `ReadAllTextAsync()`, `ReadAllLinesAsync()`, \u0914\u0930 `StreamReader` \u0915\
  \u093E `ReadLineAsync()` \u092E\u0947\u0925\u0921 \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u093F\u092F\u0947 \u091C\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  , \u091C\u093F\u0938\u0938\u0947 \u092C\u0921\u093C\u0940 \u092B\u093C\u093E\u0907\
  \u0932\u094B\u0902 \u0915\u094B \u092A\u0922\u093C\u0924\u0947 \u0938\u092E\u092F\
  \ \u090F\u092A\u094D\u0932\u093F\u0915\u0947\u0936\u0928 \u0930\u0947\u0938\u094D\
  \u092A\u094B\u0902\u0938\u093F\u0935 \u092C\u0928\u093E \u0930\u0939\u0924\u093E\
  \ \u0939\u0948\u0964 \u0915\u093E\u0930\u094D\u092F\u093E\u0928\u094D\u0935\u092F\
  \u0928 \u0935\u093F\u0935\u0930\u0923\u094B\u0902 \u0915\u0940 \u092C\u093E\u0924\
  \ \u0915\u0930\u0947\u0902, \u0924\u094B \u0938\u0941\u0930\u0915\u094D\u0937\u093E\
  \ \u0915\u0947 \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u0938\u0947\
  \ `StreamReader` \u0924\u092C \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u094B\u0924\
  \u093E \u0939\u0948 \u091C\u092C \u092B\u093E\u0907\u0932 \u0915\u093E \u0938\u093E\
  \u0907\u091C \u0905\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0939\u094B, \u0907\
  \u0938\u0938\u0947 \u0939\u092E\u0947\u0902 \u092E\u0947\u092E\u094B\u0930\u0940\
  \ \u092E\u0948\u0928\u0947\u091C\u092E\u0947\u0902\u091F \u092E\u0947\u0902 \u0938\
  \u0939\u093E\u092F\u0924\u093E \u092E\u093F\u0932\u0924\u0940 \u0939\u0948\u0964\
  ."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
