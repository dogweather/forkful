---
date: 2024-01-26 04:44:59.819421-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: PowerShell \u092E\
  \u0947\u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\
  \u0902 \u0915\u0947 \u0932\u093F\u090F \u0928\u093F\u0930\u094D\u092E\u093F\u0924\
  \ \u0938\u0939\u092F\u094B\u0917 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0907\u0938\
  \u0932\u093F\u090F \u0906\u092A \u092F\u093E \u0924\u094B \u0905\u092A\u0928\u093E\
  \ \u0938\u094D\u0935\u092F\u0902 \u0915\u093E \u0938\u092E\u093E\u0927\u093E\u0928\
  \ \u092C\u0928\u093E\u0924\u0947 \u0939\u0948\u0902 \u092F\u093E .NET \u0915\u093E\
  \ `System.Numerics.Complex`\u2026"
lastmod: '2024-03-13T22:44:52.688580-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u092E\u0947\u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\
  \u094D\u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u0928\u093F\u0930\
  \u094D\u092E\u093F\u0924 \u0938\u0939\u092F\u094B\u0917 \u0928\u0939\u0940\u0902\
  \ \u0939\u0948, \u0907\u0938\u0932\u093F\u090F \u0906\u092A \u092F\u093E \u0924\u094B\
  \ \u0905\u092A\u0928\u093E \u0938\u094D\u0935\u092F\u0902 \u0915\u093E \u0938\u092E\
  \u093E\u0927\u093E\u0928 \u092C\u0928\u093E\u0924\u0947 \u0939\u0948\u0902 \u092F\
  \u093E .NET \u0915\u093E `System.Numerics.Complex` \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 14
---

## कैसे करें:
PowerShell में जटिल संख्याओं के लिए निर्मित सहयोग नहीं है, इसलिए आप या तो अपना स्वयं का समाधान बनाते हैं या .NET का `System.Numerics.Complex` उपयोग करते हैं।

```PowerShell
# चलो .NET का उपयोग करके जटिल संख्याओं को बनाते हैं
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# जटिल संख्याएँ बनाएँ
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# दो जटिल संख्याओं को जोड़ें
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# दो जटिल संख्याओं को गुणा करें
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# परिणाम प्रदर्शित करें
"Sum: $sum"
"Product: $product"
```
आउटपुट:
```
Sum: (4, 6)
Product: (-5, 10)
```

## गहन जानकारी
जटिल संख्याएँ 16वीं शताब्दी में विकसित की गई थी ताकि उन समीकरणों को हल किया जा सके जिनके वास्तविक संख्याओं के क्षेत्र में हल नहीं थे। वे अब आधुनिक गणित का एक आधार हैं।

PowerShell का .NET पर जटिल संख्या समर्थन पर निर्भरता का मतलब है कि प्रदर्शन मजबूत है। विकल्पों में तृतीय-पक्ष लाइब्रेरियाँ या अन्य प्रोग्रामिंग भाषाएँ शामिल हैं जैसे कि Python, जहाँ जटिल संख्याएँ एक मूल डेटा प्रकार हैं।

## देखें भी
- [System.Numerics.Complex संरचना](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Python में जटिल संख्या अंकगणित](https://docs.python.org/3/library/cmath.html)
