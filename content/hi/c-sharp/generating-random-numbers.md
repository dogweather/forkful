---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-27T20:34:07.908803-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

C# में यादृच्छिक संख्याएँ उत्पन्न करना एक निर्धारित श्रेणी के भीतर अप्रत्याशित सांख्यिकीय मूल्यों की सृष्टि करने की प्रक्रिया है। प्रोग्रामर इन विधियों का उपयोग क्रिप्टोग्राफी, सिमुलेशन और खेलों जैसी सुविधाओं को लागू करने के लिए करते हैं जहाँ अप्रत्याशितता या वास्तविक दुनिया की यादृच्छिकता का अनुकरण आवश्यक होता है।

## कैसे करें:

C# में यादृच्छिक संख्याएँ उत्पन्न करने का सबसे सामान्य तरीका `System.Random` क्लास का उपयोग करना है। यह एक सरल उदाहरण है जो इसके उपयोग को दर्शाता है:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 1 और 99 के बीच एक संख्या उत्पन्न करता है
        Console.WriteLine($"यादृच्छिक संख्या: {randomNumber}");
    }
}
```

इससे एक यादृच्छिक संख्या जैसे कि:

```
यादृच्छिक संख्या: 42
```

0.0 और 1.0 के बीच एक यादृच्छिक फ्लोटिंग-पॉइंट संख्या उत्पन्न करने के लिए, आप `NextDouble` मेथड का उपयोग कर सकते हैं:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"यादृच्छिक डबल: {randomDouble}");
```

यदि आप एक सुरक्षा-संवेदनशील अप्लिकेशन पर काम कर रहे हैं जिसे क्रिप्टोग्राफिक यादृच्छिकता की आवश्यकता है, तो `System.Security.Cryptography` में पाए जाने वाले `RNGCryptoServiceProvider` क्लास का उपयोग करना बेहतर है:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // 4-बाइट लंबी यादृच्छिक संख्या बनाता है
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"क्रिप्टोग्राफिक रूप से सुरक्षित यादृच्छिक संख्या: {value}");
    }
}
```

## गहराई में:

C# में यादृच्छिक संख्या उत्पादन के सालों से विकसित हुआ है। प्रारंभ में, `System.Random` क्लास पीसाउडो-रैंडम संख्याओं को उत्पन्न करने के लिए जाने-माने विकल्प थे। यह पीसाउडो-रैंडम है क्योंकि, एक निश्चित बीज मूल्य दिए गए, यह एक ही अनुक्रम की संख्याओं को उत्पन्न करेगा, जो डिबगिंग या परीक्षणों की पुनरावृत्ति के लिए उपयोगी हो सकता है।

जबकि बुनियादी जरूरतों के लिए पर्याप्त, `System.Random` धागा-सुरक्षित (thread-safe) नहीं है और पूर्वानुमानयोग्य परिणाम पैदा कर सकता है, जो सुरक्षा-निर्भर अप्लिकेशनों के लिए उपयुक्त नहीं है। इस सीमा ने क्रिप्टोग्राफिक यादृच्छिकता के लिए `RNGCryptoServiceProvider` की परिचय दिया, जो अधिक सुरक्षित है लेकिन साथ ही अधिक संसाधन-गहन है।

.NET Core और .NET 5+ में एक विकल्प `System.Security.Cryptography` में `RandomNumberGenerator` क्लास है, जो सुरक्षित रूप से यादृच्छिक संख्याओं को उत्पन्न करने के लिए एक अधिक आधुनिक और उपयोग में आसान विकल्प के रूप में है `RNGCryptoServiceProvider` की तुलना में।

C# में यादृच्छिक संख्याएं उत्पन्न करने की प्रत्येक विधि का अनुप्रयोग की आवश्यकताओं के आधार पर अपना स्थान है। अधिकांश अप्लिकेशनों के लिए, `System.Random` पर्याप्त है, लेकिन उनके लिए जो सुरक्षित, अप्रत्याशित यादृच्छिक संख्याओं की आवश्यकता है, क्रिप्टोग्राफिक क्लास एक मजबूत विकल्प प्रदान करते हैं।
