---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:30.489367-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: **\u092E\u0942\u0932\
  \ \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917:** `DateTime.Parse` \u0914\u0930\
  \ `DateTime.TryParse` \u0935\u093F\u0927\u093F\u092F\u093E\u0901 \u090F\u0915 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B `DateTime` \u092E\u0947\u0902\
  \ \u092C\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092A\u0938\u0902\
  \u0926\u0940\u0926\u093E \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0902\
  \u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0924\u094D\u0935\u0930\u093F\u0924\
  \u2026"
lastmod: '2024-04-05T21:53:54.345638-06:00'
model: gpt-4-0125-preview
summary: "**\u092E\u0942\u0932 \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917:**\
  \ `DateTime.Parse` \u0914\u0930 `DateTime.TryParse` \u0935\u093F\u0927\u093F\u092F\
  \u093E\u0901 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u094B `DateTime` \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u092A\u0938\u0902\u0926\u0940\u0926\u093E \u0935\u093F\u0915\
  \u0932\u094D\u092A \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915\
  \ \u0924\u094D\u0935\u0930\u093F\u0924 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\
  \u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे करें:
**मूल पार्सिंग:**

`DateTime.Parse` और `DateTime.TryParse` विधियाँ एक स्ट्रिंग को `DateTime` में बदलने के लिए पसंदीदा विकल्प हैं। यहाँ एक त्वरित उदाहरण है:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"सफलतापूर्वक पार्स किया गया: {parsedDate}");
}
else
{
    Console.WriteLine("पार्स करने में असफल।");
}
// आउटपुट: सफलतापूर्वक पार्स किया गया: 4/12/2023 12:00:00 AM
```

**सांस्कृतिक रूप से निर्दिष्ट करना:**

कभी-कभी, आपको एक विशेष संस्कृति प्रारूप में दिनांक स्ट्रिंग पार्स करने की आवश्यकता होती है। आप इसे `CultureInfo` क्लास का उपयोग करके हासिल कर सकते हैं:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// आउटपुट: 4/12/2023 12:00:00 AM
```

**विशेष प्रारूप के साथ सटीक पार्सिंग:**

जिन परिदृश्यों में दिनांक विशेष प्रारूप में आते हैं जो मानक नहीं हो सकते, `DateTime.ParseExact` काम में आता है:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// आउटपुट: 4/12/2023 12:00:00 AM
```

**NodaTime का उपयोग करना:**

और भी अधिक मजबूत दिनांक और समय पार्सिंग के लिए, प्रसिद्ध तृतीय-पक्ष पुस्तकालय NodaTime का उपयोग करें। यह दिनांक/समय संभालने की एक विस्तृत श्रेणी की क्षमताओं को प्रदान करता है:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("पार्स करने में असफल।");
}
```

NodaTime समय क्षेत्रों, अवधि और कालावधि अवधारणाओं, और अनेक अलग-अलग कैलेंडर प्रणालियों के लिए व्यापक समर्थन प्रदान करता है, जो इसे .NET अनुप्रयोगों में जटिल दिनांक और समय में हेरफेर के लिए एक शक्तिशाली चुनाव बनाता है।
