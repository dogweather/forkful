---
date: 2024-01-20 17:36:53.796949-07:00
description: "C# \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092A\u0930\u093F\
  \u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\
  \u092C \u0924\u093E\u0930\u0940\u0916 \u0915\u0947 \u0921\u0947\u091F\u093E \u0915\
  \u094B \u092A\u0920\u0928\u0940\u092F \u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u092B\u0949\u0930\u094D\u092E \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u0921\u0947\
  \u091F\u093E \u0915\u094B \u092F\u0942\u091C\u0930\u2026"
lastmod: '2024-03-13T22:44:52.351760-06:00'
model: gpt-4-1106-preview
summary: "C# \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092A\u0930\u093F\
  \u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\
  \u092C \u0924\u093E\u0930\u0940\u0916 \u0915\u0947 \u0921\u0947\u091F\u093E \u0915\
  \u094B \u092A\u0920\u0928\u0940\u092F \u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u092B\u0949\u0930\u094D\u092E \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u0921\u0947\
  \u091F\u093E \u0915\u094B \u092F\u0942\u091C\u0930\u2026"
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## What & Why? (क्या और क्यों?)
C# में तारीख को स्ट्रिंग में परिवर्तित करना मतलब तारीख के डेटा को पठनीय टेक्स्ट फॉर्म में बदलना है। प्रोग्रामर इसे इसलिए करते हैं क्योंकि यह डेटा को यूजर फ्रेंडली ढंग से दिखाने, लॉग फाइल्स में स्टोर करने या वेब पेज पर रेंडर करने में मदद करता है।

## How to: (कैसे करें:)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        
        // डिफ़ॉल्ट फॉर्मेट में डेट को स्ट्रिंग में बदलना
        string dateStringDefault = currentDate.ToString();
        Console.WriteLine(dateStringDefault); // "01-04-2023 12:34:56 PM"
        
        // कस्टम फॉर्मेट में डेट को स्ट्रिंग में बदलना
        string dateStringCustom = currentDate.ToString("dd/MM/yyyy");
        Console.WriteLine(dateStringCustom); // "01/04/2023"
        
        // कल्चर स्पेसिफिक फॉर्मेट में डेट को स्ट्रिंग में बदलना
        string dateStringCulture = currentDate.ToString(CultureInfo.CreateSpecificCulture("hi-IN"));
        Console.WriteLine(dateStringCulture); // "शनिवार, 1 अप्रैल 2023"
    }
}
```

## Deep Dive (गहरी जानकारी)
C# में जब `.ToString()` का इस्तेमाल किया जाता है, तो इसे फॉर्मेट स्ट्रिंग्स के साथ कस्टमाइज़ किया जा सकता है। पहले-पहले, C# 1.0 में सिर्फ कुछ सिम्पल फॉर्मेटिंग विकल्प थे, लेकिन धीरे-धीरे अधिक जटिल पैटर्न की सुविधाएँ भी जोड़ी गईं।

वैकल्पिक तरीकों में `String.Format()` और इंटरपोलेशन (`$"{}"`) शामिल हैं। `.ToString()` और `String.Format()` दोनों ही `IFormatProvider` ले सकते हैं, जैसे कि एक `CultureInfo` ऑब्जेक्ट, जो अलग-अलग कल्चरल कॉन्वेंशन्स के अनुसार टेक्स्ट फॉर्मैटिंग को संभव बनाता है।

अंतरराष्ट्रीयकरण के संदर्भ में यह बहुत महत्त्वपूर्ण होता है, जहाँ एक ही एप्लीकेशन को विभिन्न भाषाओं और लोकेल्स के लिए अनुकूलित करना पड़ता है। 

`ToString()` की इम्प्लीमेंटेशन `DateTime` के अंदर `IFormattable` इंटरफेस के द्वारा होती है, जो फॉर्मेटिंग सर्विसेज के लिए एक आम आधार प्रदान करता है।

## See Also (और जानकारी के लिए)
- [DateTime.ToString() Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
