---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:14:09.738123-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

मौजूदा तारीख को प्राप्त करने का मतलब है वह तिथि जो आज की है। प्रोग्रामर्स इसे लॉगिंग, टाइमस्टैंप्स, फीचर्स को एक्टिवेट या डिएक्टिवेट करने और यूजर्स को समय से संबंधित जानकारी देने के लिए करते हैं। 

## How to: (कैसे करें?)

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now; // मौजूदा समय और तारीख
        Console.WriteLine(currentDate.ToShortDateString()); // केवल तारीख दिखाएँ
    }
}
```

उपरोक्त कोड को जब चलाएंगे, आउटपुट कुछ इस प्रकार होगा:

```
10-04-2023
```

## Deep Dive (गहराई से समझें)

DateTime.Now प्रॉपर्टी सिस्टम क्लॉक का इस्तेमाल करके मौजूदा तारीख और समय देती है। यह .NET फ्रेमवर्क का हिस्सा है और C# समेत .NET लैंग्वेजेस में बहुप्रचलित है। UtcNow और Today जैसे विकल्प भी हैं जो क्रमशः UTC समय और केवल मौजूदा तारीख (समय के बिना) प्रदान करते हैं।

डेट-टाइम डेटा को संभालना कभी-कभी जटिल हो सकता है, जैसे टाइम ज़ोन के बीच परिवर्तन, डेलाइट सेविंग टाइम एडजस्टमेंट्स, और कैलेंडर सिस्टम की विविधताएं। इन्हें संभालने के लिए, .NET अन्य वर्ग जैसे TimeZoneInfo और DateTimeOffset प्रदान करता है।

इतिहास में जाएं तो, DateTime वर्ग .NET के पहले संस्करण से ही मौजूद है, और इसे समय के साथ सुधारा गया है। हालांकि, DateTime की अपनी सीमाएँ हैं, और कुछ परिस्थितियों में, थर्ड-पार्टी लाइब्रेरीज जैसे कि NodaTime का इस्तेमाल अधिक उपयुक्त हो सकता है।

## See Also (और जानकारी)

- [DateTime Struct (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Handling Dates and Times in .NET (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- [NodaTime Library](https://nodatime.org/)
