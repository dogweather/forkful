---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:58.319108-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C# `.NET Framework`\
  \ \u0915\u0947 `System` \u0928\u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u0915\u093E\
  \ \u0939\u093F\u0938\u094D\u0938\u093E \u092C\u0928\u0940 `DateTime` \u0915\u094D\
  \u0932\u093E\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0926\u093F\u0928\u093E\u0902\
  \u0915 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\
  \u093E \u090F\u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E\u2026"
lastmod: '2024-03-13T22:44:52.349976-06:00'
model: gpt-4-0125-preview
summary: "C# `.NET Framework` \u0915\u0947 `System` \u0928\u0947\u092E\u0938\u094D\
  \u092A\u0947\u0938 \u0915\u093E \u0939\u093F\u0938\u094D\u0938\u093E \u092C\u0928\
  \u0940 `DateTime` \u0915\u094D\u0932\u093E\u0938 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 \u0935\u0930\u094D\u0924\u092E\u093E\u0928\
  \ \u0926\u093F\u0928\u093E\u0902\u0915 \u092A\u094D\u0930\u093E\u092A\u094D\u0924\
  \ \u0915\u0930\u0928\u0947 \u0915\u093E \u090F\u0915 \u0938\u0930\u0932 \u0924\u0930\
  \u0940\u0915\u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948\u0964 \u0928\u0940\u091A\u0947 \u0926\u093F\u092F\u093E \u0917\u092F\
  \u093E \u0909\u0926\u093E\u0939\u0930\u0923 \u0935\u0930\u094D\u0924\u092E\u093E\
  \u0928 \u0926\u093F\u0928\u093E\u0902\u0915, \u0914\u0930 \u0935\u0948\u0915\u0932\
  \u094D\u092A\u093F\u0915 \u0930\u0942\u092A \u0938\u0947, \u0938\u092E\u092F \u092A\
  \u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u093E \u0924\
  \u0930\u0940\u0915\u093E \u0926\u093F\u0916\u093E\u0924\u093E \u0939\u0948\u0964\
  ."
title: "\u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E"
weight: 29
---

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
