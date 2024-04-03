---
date: 2024-01-26 01:01:23.866392-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C# \u092E\u0947\u0902\
  , \u0906\u092A \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `System.Diagnostics`\
  \ \u0928\u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u092F\u093E \u0925\u0930\u094D\
  \u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940\u091C \u091C\u0948\u0938\u0947 \u0915\u093F NLog \u092F\u093E\
  \ log4net \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 .NET Core \u092E\u0947\u0902\
  \u2026"
lastmod: '2024-03-13T22:44:52.342814-06:00'
model: gpt-4-1106-preview
summary: "C# \u092E\u0947\u0902, \u0906\u092A \u092C\u093F\u0932\u094D\u091F-\u0907\
  \u0928 `System.Diagnostics` \u0928\u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u092F\
  \u093E \u0925\u0930\u094D\u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u091C\u0948\u0938\u0947 \u0915\
  \u093F NLog \u092F\u093E log4net \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901\
  \ .NET Core \u092E\u0947\u0902 \u0909\u092A\u0932\u092C\u094D\u0927 `ILogger` \u0907\
  \u0902\u091F\u0930\u092B\u0947\u0938 \u0915\u093E \u090F\u0915 \u0924\u094D\u0935\
  \u0930\u093F\u0924 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E\
  \ \u0917\u092F\u093E \u0939\u0948."
title: "\u0932\u0949\u0917\u093F\u0902\u0917"
weight: 17
---

## कैसे करें:
C# में, आप बिल्ट-इन `System.Diagnostics` नेमस्पेस या थर्ड-पार्टी लाइब्रेरीज जैसे कि NLog या log4net का उपयोग कर सकते हैं। यहाँ .NET Core में उपलब्ध `ILogger` इंटरफेस का एक त्वरित उदाहरण दिया गया है:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("This is an informational message.");
        logger.LogWarning("This is a warning message.");
        logger.LogError("This is an error message.");
    }
}
```

नमूना आउटपुट:
```
info: Program[0]
      This is an informational message.
warn: Program[0]
      This is a warning message.
fail: Program[0]
      This is an error message.
```

## गहराई से विचार
सॉफ्टवेयर विकास में लॉगिंग का इतिहास लगभग प्रोग्रामिंग के जितना पुराना है; यह सिंपल प्रिंट स्टेटमेंट्स से लेकर जटिल, कॉन्फ़िगर करने योग्य सिस्टम्स तक विकसित हो चुका है। मूल रूप से, लॉगिंग फाइलों या कंसोल में लिखकर की जाती थी, लेकिन अब इसमें लॉग एग्रीगेशन सिस्टम्स और डिस्ट्रीब्यूटेड ट्रेसिंग प्लेटफॉर्म्स (जैसे ईएलके स्टैक या जागर) जैसी अधिक जटिल संरचनाएं भी शामिल हो गई हैं।

.NET में बिल्ट-इन लॉगिंग के विकल्पों में थर्ड-पार्टी लाइब्रेरीज शामिल हैं:
- **NLog**: बहुत वर्सटाइल और सेटअप करने में आसान, जिसमें लॉग्स के रूटिंग, फॉर्मेटिंग, और फिल्टरिंग के लिए कई सुविधाएं हैं।
- **log4net**: जावा log4j लाइब्रेरी से प्रेरित, यह XML के माध्यम से हाईली कॉन्फ़िगरेबल है और विभिन्न प्रकार के लॉग रिपॉजिटरीज का समर्थन करता है।

जब बात आती है लागू करने के विवरणों की, तो आपके लॉगिंग अब्स्ट्रक्शन (जैसे Microsoft.Extensions.Logging) और अधारभूत लॉगिंग प्रोवाइडर की पसंद आपके ऐप्लिकेशन के प्रदर्शन और विश्वसनीयता पर काफी प्रभाव डाल सकती है। लॉगिंग स्तरों को उचित रूप से कॉन्फ़िगर करना और सुनिश्चित करना कि लॉग्स लिखना एक बैरियर न बन जाए, यह महत्वपूर्ण है।

साथ ही, स्ट्रक्चर्ड लॉगिंग - जहां आप केवल स्ट्रिंग्स ही नहीं बल्कि की-वैल्यू जोड़ीयां या ऑब्जेक्ट्स - लॉग करते हैं, वह अधिक प्रेसिस और कार्रवाई योग्य लॉग्स के लिए अनुमति देता है, जिन्हें क्वेरी करना और विश्लेषण करना आसान होता है।

## और देखें
- [Microsoft.Extensions.Logging दस्तावेज़ीकरण](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog दस्तावेज़ीकरण](https://nlog-project.org/documentation/)
- [log4net दस्तावेज़ीकरण](https://logging.apache.org/log4net/)
- [Serilog दस्तावेज़ीकरण](https://serilog.net/) (स्ट्रक्चर्ड लॉगिंग का उदाहरण के लिए)
