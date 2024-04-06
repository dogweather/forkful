---
date: 2024-01-20 17:33:45.900221-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093F\
  \u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E C# \u092E\
  \u0947\u0902 `DateTime.Compare()` \u092E\u0947\u0925\u0921 \u0915\u0947 \u091C\u0930\
  \u093F\u090F \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\u0940 \u0939\u0948\u0964\
  \ 2002 \u0938\u0947, \u091C\u092C .NET Framework \u092A\u0947\u0936 \u0915\u093F\
  \u092F\u093E \u0917\u092F\u093E, \u0924\u092C \u0938\u0947 \u092F\u0939 \u092B\u0940\
  \u091A\u0930 \u092E\u093E\u0928\u0915\u2026"
lastmod: '2024-04-05T22:51:07.040417-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093F\u0925\u093F\
  \u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E C# \u092E\u0947\u0902\
  \ `DateTime.Compare()` \u092E\u0947\u0925\u0921 \u0915\u0947 \u091C\u0930\u093F\u090F\
  \ \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\u0940 \u0939\u0948\u0964 2002 \u0938\
  \u0947, \u091C\u092C .NET Framework \u092A\u0947\u0936 \u0915\u093F\u092F\u093E\
  \ \u0917\u092F\u093E, \u0924\u092C \u0938\u0947 \u092F\u0939 \u092B\u0940\u091A\u0930\
  \ \u092E\u093E\u0928\u0915 \u0930\u0939\u093E \u0939\u0948\u0964 \u0907\u0938\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0908 \u0924\u0930\u0939 \u0915\u0947\
  \ \u0938\u093F\u0938\u094D\u091F\u092E \u092E\u0947\u0902 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948\u0964 `DateTime.Compare()` \u0926\u094B `DateTime` \u0911\u092C\u094D\
  \u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u0915\u0947 \u092C\u0940\u091A \u0924\
  \u0941\u0932\u0928\u093E \u0915\u0930\u0924\u093E \u0939\u0948 \u0914\u0930 \u092A\
  \u0930\u093F\u0923\u093E\u092E \u0915\u0947 \u0930\u0942\u092A \u092E\u0947\u0902\
  \ \u090F\u0915 \u0907\u0902\u091F\u0940\u091C\u0930 \u0932\u094C\u091F\u093E\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939 `IComparable` \u0907\u0902\u091F\u0930\u092B\
  \u0947\u0938 \u0915\u0947 \u0924\u0939\u0924 \u092A\u0930\u093F\u092D\u093E\u0937\
  \u093F\u0924 \u0939\u094B\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u0947 \u0914\
  \u0930 \u092D\u0940 \u0921\u0947\u091F\u093E \u091F\u093E\u0907\u092A\u094D\u0938\
  \ \u0907\u092E\u094D\u092A\u094D\u0932\u093F\u092E\u0947\u0902\u091F \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u0935\u093F\u0915\u0932\u094D\u092A\u094B\
  \u0902 \u092E\u0947\u0902 `CompareTo()` \u0914\u0930 \u0913\u0935\u0930\u0932\u094B\
  \u0921\u0947\u0921 `==` \u0914\u0930 `!=` \u0911\u092A\u0930\u0947\u091F\u0930\u094D\
  \u0938 \u092D\u0940 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0902\u0964 \u0938\
  \u0940# \u092E\u0947\u0902, \u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u0940\
  \ \u0924\u0941\u0932\u0928\u093E \u0910\u0938\u0947 \u0939\u094B\u0924\u0940 \u0939\
  \u0948 \u0915\u093F \u0938\u092E\u092F \u0905\u0928\u0941\u0915\u094D\u0930\u092E\
  \ \u0915\u0947 \u0938\u0902\u0926\u0930\u094D\u092D \u092E\u0947\u0902 \u0938\u0939\
  \u0940 \u092A\u0930\u093F\u0923\u093E\u092E \u092E\u093F\u0932\u0947\u0964."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

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
