---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases: - /hi/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:30.489367-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में एक स्ट्रिंग से दिनांक पार्स करने का मतलब होता है दिनांकों और समयों के पाठ्य प्रतिनिधित्वों को `DateTime` ऑब्जेक्ट में बदलना। जैसे कि शेड्यूलिंग ऐप्स, लॉग प्रोसेसर्स, या कोई भी सिस्टम जो उपयोगकर्ताओं या बाहरी स्रोतों से दिनांक इनपुट संभालता है, के लिए यह आवश्यक है कि वे विभिन्न प्रारूपों में दिनांकों और समय को संभाल सकें, स्टोर कर सकें या प्रदर्शित कर सकें।

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
