---
title:                "लॉगिंग"
aliases:
- /hi/c-sharp/logging/
date:                  2024-01-26T01:01:23.866392-07:00
model:                 gpt-4-1106-preview
simple_title:         "लॉगिंग"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/logging.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

लॉगिंग एक प्रक्रिया है जिसमें ऐप्लिकेशन की घटनाओं और डाटा का आउटपुट रनटाइम के दौरान रिकॉर्ड किया जाता है। प्रोग्रामर बग्स की निदान, सॉफ्टवेयर प्रदर्शन की निगरानी, उपयोगकर्ताओं की क्रियाओं की ट्रैकिंग, और सुरक्षा व व्यापारिक मानदंडों के अनुपालन को बनाए रखने के लिए लॉग करते हैं।

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
