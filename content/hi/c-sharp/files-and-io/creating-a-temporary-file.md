---
date: 2024-01-20 17:40:54.364479-07:00
description: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\
  \ \u092C\u0928\u093E\u0928\u093E \u090F\u0915 \u0910\u0938\u0940 \u092A\u094D\u0930\
  \u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947\
  \ \u0906\u092A \u0921\u0947\u091F\u093E \u0915\u094B \u0905\u0938\u094D\u0925\u093E\
  \u092F\u0940 \u0924\u094C\u0930 \u092A\u0930 \u0938\u094D\u091F\u094B\u0930 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\
  \u093F\u0902\u0917, \u091F\u0947\u092E\u094D\u092A\u0930\u0947\u0930\u0940 \u092C\
  \u0948\u0915\u0905\u092A, \u0914\u0930 \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.365643-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\
  \ \u092C\u0928\u093E\u0928\u093E \u090F\u0915 \u0910\u0938\u0940 \u092A\u094D\u0930\
  \u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947\
  \ \u0906\u092A \u0921\u0947\u091F\u093E \u0915\u094B \u0905\u0938\u094D\u0925\u093E\
  \u092F\u0940 \u0924\u094C\u0930 \u092A\u0930 \u0938\u094D\u091F\u094B\u0930 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\
  \u093F\u0902\u0917, \u091F\u0947\u092E\u094D\u092A\u0930\u0947\u0930\u0940 \u092C\
  \u0948\u0915\u0905\u092A, \u0914\u0930 \u0921\u0947\u091F\u093E\u2026"
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

अस्थायी फ़ाइल बनाना एक ऐसी प्रक्रिया है जिससे आप डेटा को अस्थायी तौर पर स्टोर करते हैं। प्रोग्रामर्स इसका उपयोग डेटा प्रोसेसिंग, टेम्परेरी बैकअप, और डेटा एक्सचेंज करते समय करते हैं।

## How to (कैसे करें):

C# में अस्थायी फ़ाइल बनाना बहुत सरल है। नीचे एक साधारण उदाहरण दिया गया है:

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tempFilePath = Path.GetTempFileName();
        Console.WriteLine("Temporary File Created: " + tempFilePath);

        // फाइल में कुछ लिखिए
        File.WriteAllText(tempFilePath, "Hello, this is some temp data!");

        // फाइल से डेटा पढ़िए
        string readText = File.ReadAllText(tempFilePath);
        Console.WriteLine("Data in Temporary File: " + readText);

        // याद रखें कि फाइल को डिलीट कर देना है
        File.Delete(tempFilePath);
        Console.WriteLine("Temporary File Deleted.");
    }
}
```

जब आप यह कोड रन करेंगे तो आपको कुछ ऐसा आउटपुट मिलेगा:

```
Temporary File Created: C:\Users\...\AppData\Local\Temp\tmp1E4.tmp
Data in Temporary File: Hello, this is some temp data!
Temporary File Deleted.
```

## Deep Dive (गहराई में जानकारी):

इतिहास में, प्रोग्रामर्स हमेशा अस्थायी फ़ाइलों का उपयोग करते रहे हैं क्योंकि इससे कार्यक्षमता और प्रोग्राम की दक्षता बढ़ती है। C# `Path.GetTempFileName()` मेथड सिस्टम के अस्थायी फोल्डर में एक यूनिक फ़ाइल नाम बनाता है। फाइल पथ और नाम सुरक्षित रूप से उत्पन्न होते हैं, जिससे यह सुनिश्चित होता है कि फाइल नाम की टकराव न हो।

एक वैकल्पिक तरीका है अस्थायी स्ट्रीम्स का उपयोग, `Path.GetRandomFileName()` या `Guid.NewGuid()` उपयोग करके एक यूनिक फ़ाइल नाम उत्पन्न कर सकते हैं, और इसे टेम्प डायरेक्टरी में मैन्युअली जोड़ सकते हैं। हालांकि, `GetTempFileName()` सबसे सरल और सीधा तरीका है।

हमेशा ध्यान रखें कि अस्थायी फ़ाइलों को उपयोग के बाद डिलीट कर देना चाहिए, नहीं तो वह सिस्टम पर अनावश्यक जगह घेर सकती हैं।

## See Also (अन्य देखें):

- Microsoft Docs पर [`Path.GetTempFileName()`](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- Microsoft Docs पर [`File.WriteAllText`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext)
- Microsoft Docs पर [`File.ReadAllText`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)
