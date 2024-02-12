---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- /hi/c-sharp/writing-a-text-file/
date:                  2024-02-03T19:28:26.126538-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में टेक्स्ट फाइल लिखना यानी प्रोग्राम के जरिये फाइल सिस्टम पर टेक्स्ट फाइलों को बनाना या मोडिफाई करना शामिल है - यह कई एप्लिकेशन्स के लिए एक मूलभूत कार्य है, जैसे कि लॉगिंग, डेटा निर्यात करना, या कॉन्फ़िगरेशन प्रबंधन। प्रोग्रामर्स इस ऑपरेशन को सत्रों के बीच डेटा संग्रहित करने, सिस्टमों के आर-पार जानकारी साझा करने, या मानव-पठनीय आउटपुट्स संग्रहित करने के लिए करते हैं।

## कैसे:
C# अपने `System.IO` नेमस्पेस के साथ फाइल ऑपरेशन्स को सरल बनाता है, जो टेक्स्ट फाइलें लिखने के लिए सीधे-सादे तरीके प्रदान करता है। यहाँ एक बुनियादी टेक्स्ट फाइल लिखने और एक मौजूदा फाइल में टेक्स्ट जोड़ने का तरीका है।

### शुरू से एक टेक्स्ट फाइल में लिखना
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // नई फाइल में कंटेंट लिखें
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("File सफलतापूर्वक लिखी गई।");
    }
}
```
**नमूना उत्पादन:**
```
File सफलतापूर्वक लिखी गई।
```

### मौजूदा फाइल में टेक्स्ट जोड़ना
यदि आप एक मौजूदा फाइल के अंत में टेक्स्ट जोड़ना चाहते हैं, तो आप `File.AppendAllText` मेथड का उपयोग कर सकते हैं।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nAdding more content.";

        // फाइल में कंटेंट जोड़ें
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("कंटेंट सफलतापूर्वक जोड़ा गया।");
    }
}
```
**नमूना उत्पादन:**
```
कंटेंट सफलतापूर्वक जोड़ा गया।
```

### तीसरे पक्ष की लाइब्रेरीज़ का उपयोग: `StreamWriter`
लिखावट करते समय, जिसमें स्वचालित फ्लशिंग और एन्कोडिंग चयन शामिल है, अधिक बारीक नियंत्रण के लिए `StreamWriter` का उपयोग करें।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "This is an example using StreamWriter.";

        // StreamWriter का उपयोग करके फाइल में लिखें
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("StreamWriter के साथ सफलतापूर्वक फाइल लिखी गई।");
    }
}
```
**नमूना उत्पादन:**
```
StreamWriter के साथ सफलतापूर्वक फाइल लिखी गई।
```

ये सभी दृष्टिकोण विभिन्न आवश्यकताओं की सेवा करते हैं: त्वरित ऑपरेशन्स के लिए सीधे `File` मेथड्स, और अधिक जटिल लेखन परिदृश्यों के लिए `StreamWriter`. अपनी विशिष्ट आवश्यकताओं के आधार पर चयन करें, प्रदर्शन और फाइल के आकार जैसे कारकों पर विचार करते हुए।
