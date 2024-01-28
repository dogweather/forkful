---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-01-26T04:44:59.819421-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वास्तविक भाग और काल्पनिक भाग (जैसे 3 + 4i) वाली संख्याएँ, जो कि जटिल संख्याएँ होती हैं, इंजीनियरिंग, भौतिकी और डेटा विज्ञान जैसे क्षेत्रों में महत्वपूर्ण होती हैं। प्रोग्रामर्स इनका उपयोग सिमुलेशन, सिग्नल प्रोसेसिंग, और विशिष्ट प्रकार की गणित समस्याओं को हल करने में करते हैं।

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
