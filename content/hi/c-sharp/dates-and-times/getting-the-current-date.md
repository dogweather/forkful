---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:58.319108-07:00
description: "C# \u092E\u0947\u0902 \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0926\
  \u093F\u0928\u093E\u0902\u0915 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\
  \u0930\u0928\u093E \u0938\u093F\u0938\u094D\u091F\u092E \u0938\u0947 \u0935\u0930\
  \u094D\u0924\u092E\u093E\u0928 \u0926\u093F\u0928\u093E\u0902\u0915 \u0914\u0930\
  \ \u0938\u092E\u092F \u0915\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0932\
  \u0947\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0915\u094B \u0905\u0915\u094D\u0938\u0930 \u0932\u0949\u0917\
  \u093F\u0902\u0917, \u0938\u092E\u092F \u091A\u093F\u0928\u094D\u0939\u093F\u0924\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940\u2026"
lastmod: 2024-02-19 22:05:11.357335
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0926\u093F\
  \u0928\u093E\u0902\u0915 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\
  \u0928\u093E \u0938\u093F\u0938\u094D\u091F\u092E \u0938\u0947 \u0935\u0930\u094D\
  \u0924\u092E\u093E\u0928 \u0926\u093F\u0928\u093E\u0902\u0915 \u0914\u0930 \u0938\
  \u092E\u092F \u0915\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0932\u0947\
  \u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \u094D\u0938 \u0915\u094B \u0905\u0915\u094D\u0938\u0930 \u0932\u0949\u0917\u093F\
  \u0902\u0917, \u0938\u092E\u092F \u091A\u093F\u0928\u094D\u0939\u093F\u0924 \u0915\
  \u0930\u0928\u0947 \u0915\u0940\u2026"
title: "\u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में वर्तमान दिनांक प्राप्त करना सिस्टम से वर्तमान दिनांक और समय की जानकारी लेने की प्रक्रिया है। प्रोग्रामर्स को अक्सर लॉगिंग, समय चिन्हित करने की क्रियाओं, या एप्लिकेशनों में कार्यों को शेड्यूलिंग के लिए इस जानकारी की आवश्यकता होती है, जिससे यह सुनिश्चित होता है कि कार्य सही समय पर किए जाते हैं और डाटा सटीक समय चिन्हों के साथ चिह्नित होता है।

## कैसे करें:
C# `.NET Framework` के `System` नेमस्पेस का हिस्सा बनी `DateTime` क्लास का उपयोग करके वर्तमान दिनांक प्राप्त करने का एक सरल तरीका प्रदान करता है। नीचे दिया गया उदाहरण वर्तमान दिनांक, और वैकल्पिक रूप से, समय प्राप्त करने का तरीका दिखाता है।

```csharp
using System;

class Program
{
    static void Main()
    {
        // केवल वर्तमान दिनांक प्राप्त करता है
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // परिणाम: MM/dd/yyyy
        
        // वर्तमान दिनांक और समय प्राप्त करता है
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // परिणाम: MM/dd/yyyy HH:mm:ss

        // वर्तमान UTC दिनांक और समय प्राप्त करता है
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // परिणाम: MM/dd/yyyy HH:mm:ss
    }
}
```

तृतीय-पक्ष पुस्तकालयों के संदर्भ में, NodaTime विभिन्न कैलेंडरों और समय क्षेत्रों में वर्तमान दिनांक प्राप्त करने के लिए तिथि और समय परिवर्तन के लिए एक मजबूत विकल्प प्रदान करता है।

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // ISO कैलेंडर में वर्तमान दिनांक प्राप्त करने के लिए NodaTime का उपयोग करता है
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // परिणाम: yyyy-MM-dd

        // समय क्षेत्र-विशेष दिनांक के लिए
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // परिणाम: yyyy-MM-dd
    }
}
```

यह बिल्ट-इन `DateTime` क्लास के साथ मूलभूत उपयोग और NodaTime द्वारा प्रदान की गई विशेष क्षमताओं को दर्शाता है, विशेष रूप से उन ऍप्लिकेशनों के लिए उपयोगी जिन्हें विभिन्न समय क्षेत्रों या कैलेंडर प्रणालियों को संभालने की आवश्यकता होती है।
