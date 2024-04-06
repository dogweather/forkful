---
date: 2024-01-20 17:36:53.796949-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u092E\
  \u0947\u0902 \u091C\u092C `.ToString()` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\
  , \u0924\u094B \u0907\u0938\u0947 \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u0938\u093E\
  \u0925 \u0915\u0938\u094D\u091F\u092E\u093E\u0907\u091C\u093C \u0915\u093F\u092F\
  \u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092A\u0939\u0932\
  \u0947-\u092A\u0939\u0932\u0947, C# 1.0 \u092E\u0947\u0902\u2026"
lastmod: '2024-04-05T22:51:07.038398-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u092E\u0947\u0902\
  \ \u091C\u092C `.ToString()` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u0924\u094B\
  \ \u0907\u0938\u0947 \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u0938\u094D\u091F\u092E\u093E\u0907\u091C\u093C \u0915\u093F\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092A\u0939\u0932\u0947-\u092A\
  \u0939\u0932\u0947, C# 1.0 \u092E\u0947\u0902 \u0938\u093F\u0930\u094D\u092B \u0915\
  \u0941\u091B \u0938\u093F\u092E\u094D\u092A\u0932 \u092B\u0949\u0930\u094D\u092E\
  \u0947\u091F\u093F\u0902\u0917 \u0935\u093F\u0915\u0932\u094D\u092A \u0925\u0947\
  , \u0932\u0947\u0915\u093F\u0928 \u0927\u0940\u0930\u0947-\u0927\u0940\u0930\u0947\
  \ \u0905\u0927\u093F\u0915 \u091C\u091F\u093F\u0932 \u092A\u0948\u091F\u0930\u094D\
  \u0928 \u0915\u0940 \u0938\u0941\u0935\u093F\u0927\u093E\u090F\u0901 \u092D\u0940\
  \ \u091C\u094B\u0921\u093C\u0940 \u0917\u0908\u0902\u0964 \u0935\u0948\u0915\u0932\
  \u094D\u092A\u093F\u0915 \u0924\u0930\u0940\u0915\u094B\u0902 \u092E\u0947\u0902\
  \ `String.Format()` \u0914\u0930 \u0907\u0902\u091F\u0930\u092A\u094B\u0932\u0947\
  \u0936\u0928 (`$\"{}\"`) \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0902\u0964\
  \ `.ToString()` \u0914\u0930 `String.Format()` \u0926\u094B\u0928\u094B\u0902 \u0939\
  \u0940 `IFormatProvider` \u0932\u0947 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  , \u091C\u0948\u0938\u0947 \u0915\u093F \u090F\u0915 `CultureInfo` \u0911\u092C\u094D\
  \u091C\u0947\u0915\u094D\u091F, \u091C\u094B \u0905\u0932\u0917-\u0905\u0932\u0917\
  \ \u0915\u0932\u094D\u091A\u0930\u0932 \u0915\u0949\u0928\u094D\u0935\u0947\u0902\
  \u0936\u0928\u094D\u0938 \u0915\u0947 \u0905\u0928\u0941\u0938\u093E\u0930 \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u092B\u0949\u0930\u094D\u092E\u0948\u091F\u093F\
  \u0902\u0917 \u0915\u094B \u0938\u0902\u092D\u0935 \u092C\u0928\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 \u0905\u0902\u0924\u0930\u0930\u093E\u0937\u094D\u091F\u094D\
  \u0930\u0940\u092F\u0915\u0930\u0923 \u0915\u0947 \u0938\u0902\u0926\u0930\u094D\
  \u092D \u092E\u0947\u0902 \u092F\u0939 \u092C\u0939\u0941\u0924 \u092E\u0939\u0924\
  \u094D\u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948, \u091C\u0939\u093E\u0901 \u090F\u0915 \u0939\u0940 \u090F\u092A\u094D\
  \u0932\u0940\u0915\u0947\u0936\u0928 \u0915\u094B \u0935\u093F\u092D\u093F\u0928\
  \u094D\u0928 \u092D\u093E\u0937\u093E\u0913\u0902 \u0914\u0930 \u0932\u094B\u0915\
  \u0947\u0932\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u0905\u0928\u0941\u0915\
  \u0942\u0932\u093F\u0924 \u0915\u0930\u0928\u093E \u092A\u0921\u093C\u0924\u093E\
  \ \u0939\u0948\u0964 `ToString()` \u0915\u0940 \u0907\u092E\u094D\u092A\u094D\u0932\
  \u0940\u092E\u0947\u0902\u091F\u0947\u0936\u0928 `DateTime` \u0915\u0947 \u0905\u0902\
  \u0926\u0930 `IFormattable` \u0907\u0902\u091F\u0930\u092B\u0947\u0938 \u0915\u0947\
  \ \u0926\u094D\u0935\u093E\u0930\u093E \u0939\u094B\u0924\u0940 \u0939\u0948, \u091C\
  \u094B \u092B\u0949\u0930\u094D\u092E\u0947\u091F\u093F\u0902\u0917 \u0938\u0930\
  \u094D\u0935\u093F\u0938\u0947\u091C \u0915\u0947 \u0932\u093F\u090F \u090F\u0915\
  \ \u0906\u092E \u0906\u0927\u093E\u0930 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\
  \u0930\u0924\u093E \u0939\u0948\u0964."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

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
