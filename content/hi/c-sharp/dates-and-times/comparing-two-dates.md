---
date: 2024-01-20 17:33:45.900221-07:00
description: "\u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\
  \u0932\u0928\u093E \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B\
  \ \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0906\u092A\u0938 \u092E\
  \u0947\u0902 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\u0928\u093E \u0939\u0948\
  \ \u0915\u093F \u0915\u094C\u0928 \u0938\u0940 \u092A\u0939\u0932\u0947 \u0939\u0948\
  \ \u092F\u093E \u092C\u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u0926\u094B\u0928\
  \u094B\u0902 \u092C\u0930\u093E\u092C\u0930 \u0939\u0948\u0902 \u0915\u093F \u0928\
  \u0939\u0940\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \u094D\u0938 \u0907\u0938\u0947 \u0918\u091F\u0928\u093E\u0913\u0902 \u0915\u0940\
  \u2026"
lastmod: 2024-02-19 22:05:11.360896
model: gpt-4-1106-preview
summary: "\u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B \u0924\
  \u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0906\u092A\u0938 \u092E\u0947\
  \u0902 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\u0928\u093E \u0939\u0948 \u0915\
  \u093F \u0915\u094C\u0928 \u0938\u0940 \u092A\u0939\u0932\u0947 \u0939\u0948 \u092F\
  \u093E \u092C\u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u0926\u094B\u0928\u094B\
  \u0902 \u092C\u0930\u093E\u092C\u0930 \u0939\u0948\u0902 \u0915\u093F \u0928\u0939\
  \u0940\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u0907\u0938\u0947 \u0918\u091F\u0928\u093E\u0913\u0902 \u0915\u0940\u2026"
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तिथियों की तुलना करना मतलब दो तारीखों को आपस में तुलना करना है कि कौन सी पहले है या बाद में, या दोनों बराबर हैं कि नहीं। प्रोग्रामर्स इसे घटनाओं की प्रक्रिया, टाइमशीट्स का मैनेजमेंट, या अनुसूचियों की योजना बनाने के लिए करते हैं।

## How to: (कैसे करें:)

```C#
using System;

class Program {
    static void Main() {
        DateTime firstDate = new DateTime(2023, 3, 15);
        DateTime secondDate = new DateTime(2023, 4, 20);

        int comparison = DateTime.Compare(firstDate, secondDate);

        if (comparison < 0) {
            Console.WriteLine("पहली तारीख दूसरी तारीख से पहले है।");
        } else if (comparison > 0) {
            Console.WriteLine("पहली तारीख दूसरी तारीख के बाद है।");
        } else {
            Console.WriteLine("दोनों तारीखें बराबर हैं।");
        }
    }
}
```

सैंपल आउटपुट:
```
पहली तारीख दूसरी तारीख से पहले है।
```

## Deep Dive (गहराई से जानकारी):

तिथियों की तुलना C# में `DateTime.Compare()` मेथड के जरिए की जा सकती है। 2002 से, जब .NET Framework पेश किया गया, तब से यह फीचर मानक रहा है। इसका उपयोग कई तरह के सिस्टम में होता है। `DateTime.Compare()` दो `DateTime` ऑब्जेक्ट्स के बीच तुलना करता है और परिणाम के रूप में एक इंटीजर लौटाता है। यह `IComparable` इंटरफेस के तहत परिभाषित होता है, जिसे और भी डेटा टाइप्स इम्प्लिमेंट करते हैं। विकल्पों में `CompareTo()` और ओवरलोडेड `==` और `!=` ऑपरेटर्स भी शामिल हैं। सी# में, तिथियों की तुलना ऐसे होती है कि समय अनुक्रम के संदर्भ में सही परिणाम मिले।

## See Also (और देखें):

- Microsoft Documentation on `DateTime`: [DateTime Struct (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
- C# Documentation on `IComparable` Interface: [IComparable Interface (System)](https://docs.microsoft.com/en-us/dotnet/api/system.icomparable?view=net-7.0)
- Stack Overflow Discussion on Date Comparison: [Comparing Dates in C#](https://stackoverflow.com/questions/1447326/comparing-dates-in-c-sharp)
