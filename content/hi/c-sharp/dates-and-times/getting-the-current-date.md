---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-02-03T19:09:58.319108-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
