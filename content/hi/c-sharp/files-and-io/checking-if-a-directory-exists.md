---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:12.024645-07:00
description: "\u0915\u0948\u0938\u0947: C# \u092E\u0947\u0902 `System.IO` \u0928\u093E\
  \u092E\u0938\u094D\u0925\u093E\u0928 `Directory` \u0915\u094D\u0932\u093E\u0938\
  \ \u0915\u094B \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948, \u091C\u094B `Exists` \u092E\u0947\u0925\u0921 \u0915\u0947 \u092E\u093E\
  \u0927\u094D\u092F\u092E \u0938\u0947 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\
  \u093F\u0915\u093E \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\u094D\u0935\
  \ \u0915\u0940 \u091C\u093E\u0902\u091A \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\
  \u0940\u0927\u093E \u0924\u0930\u0940\u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.356932-06:00'
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 `System.IO` \u0928\u093E\u092E\u0938\u094D\u0925\u093E\
  \u0928 `Directory` \u0915\u094D\u0932\u093E\u0938 \u0915\u094B \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u094B `Exists`\
  \ \u092E\u0947\u0925\u0921 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\
  \u0947 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E \u0915\u0947\
  \ \u0905\u0938\u094D\u0924\u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\u093E\u0902\
  \u091A \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\u0940\u0927\u093E \u0924\u0930\
  \u0940\u0915\u093E \u0909\u092A\u0932\u092C\u094D\u0927 \u0915\u0930\u093E\u0924\
  \u093E \u0939\u0948\u0964."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे:


### System.IO का उपयोग करना
C# में `System.IO` नामस्थान `Directory` क्लास को प्रदान करता है, जो `Exists` मेथड के माध्यम से निर्देशिका के अस्तित्व की जांच करने का सीधा तरीका उपलब्ध कराता है।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // यह जांचें कि निर्देशिका मौजूद है या नहीं
        bool directoryExists = Directory.Exists(directoryPath);

        // परिणाम प्रिंट करें
        Console.WriteLine("निर्देशिका मौजूद है: " + directoryExists);
    }
}
```

**नमूना आउटपुट:**

```
निर्देशिका मौजूद है: असत्य
```

यदि निर्देशिका पथ `C:\ExampleDirectory` पर मौजूद है, तो आउटपुट `सत्य` होगा।

### यूनिट परीक्षण के लिए System.IO.Abstractions का उपयोग करना
जब आपका कोड फ़ाइल सिस्टम के साथ इंटरएक्ट करता है, खासकर यूनिट परीक्षण योग्य बनाने की बात आती है, तो `System.IO.Abstractions` पैकेज एक लोकप्रिय विकल्प है। यह आपको अपने परीक्षणों में फ़ाइल सिस्टम संचालनों को अमूर्त और मॉक करने की अनुमति देता है। इस दृष्टिकोण का उपयोग करके निर्देशिका के अस्तित्व की जांच कैसे करें, यहाँ देखें:

पहले, सुनिश्चित करें कि आपने पैकेज स्थापित किया है:

```
Install-Package System.IO.Abstractions
```

फिर, आप अपनी क्लास में एक `IFileSystem` इंजेक्ट कर सकते हैं और इसका उपयोग करके निर्देशिका के अस्तित्व की जांच कर सकते हैं, जो यूनिट परीक्षण को आसान बनाता है।

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("निर्देशिका मौजूद है: " + directoryExists);
    }
}
```

**नमूना आउटपुट:**

```
निर्देशिका मौजूद है: असत्य
```

यह दृष्टिकोण आपकी एप्लिकेशन लॉजिक को सीधे फ़ाइल सिस्टम एक्सेस से अलग कर देता है, जिससे आपका कोड अधिक मॉड्यूलर, परीक्षण योग्य और अनुरक्षण योग्य बन जाता है।
