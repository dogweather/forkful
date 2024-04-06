---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:26.126538-07:00
description: "\u0915\u0948\u0938\u0947: C# \u0905\u092A\u0928\u0947 `System.IO` \u0928\
  \u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092B\
  \u093E\u0907\u0932 \u0911\u092A\u0930\u0947\u0936\u0928\u094D\u0938 \u0915\u094B\
  \ \u0938\u0930\u0932 \u092C\u0928\u093E\u0924\u093E \u0939\u0948, \u091C\u094B \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932\u0947\u0902 \u0932\
  \u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u0940\u0927\u0947\
  -\u0938\u093E\u0926\u0947 \u0924\u0930\u0940\u0915\u0947 \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901\
  \ \u090F\u0915 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940\u2026"
lastmod: '2024-03-13T22:44:52.363844-06:00'
model: gpt-4-0125-preview
summary: "C# \u0905\u092A\u0928\u0947 `System.IO` \u0928\u0947\u092E\u0938\u094D\u092A\
  \u0947\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092B\u093E\u0907\u0932 \u0911\u092A\
  \u0930\u0947\u0936\u0928\u094D\u0938 \u0915\u094B \u0938\u0930\u0932 \u092C\u0928\
  \u093E\u0924\u093E \u0939\u0948, \u091C\u094B \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092B\u093E\u0907\u0932\u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0938\u0940\u0927\u0947-\u0938\u093E\u0926\u0947 \u0924\
  \u0930\u0940\u0915\u0947 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u092C\u0941\u0928\
  \u093F\u092F\u093E\u0926\u0940 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\
  \u093E\u0907\u0932 \u0932\u093F\u0916\u0928\u0947 \u0914\u0930 \u090F\u0915 \u092E\
  \u094C\u091C\u0942\u0926\u093E \u092B\u093E\u0907\u0932 \u092E\u0947\u0902 \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u091C\u094B\u0921\u093C\u0928\u0947 \u0915\
  \u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964\n"
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

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
