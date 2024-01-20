---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग की लंबाई खोजना क्या होता है? इसका मतलब होता है कि आप पता करते हैं कि स्ट्रिंग में कितने वर्ण हैं। 
किसी स्ट्रिंग की लंबाई जानने की जरुरत हमें इसलिए पड़ती है ताकि हम डाटा को सही तरीके से प्रेसेंट और मैनेज कर सकें, और कोड डीबगिंग में सहायता करने के लिए। 

## कैसे करें:

पावरशेल में, आप .Length property का उपयोग करके किसी स्ट्रिंग की लंबाई को प्राप्त कर सकते हैं। निचे दिये गए कोड से आपको समझ में आ जायेगा।

```PowerShell
$string = 'नमस्ते'
echo $string.Length
```

रन कराते हुए, यह कोड “4” का आउटपुट देगा क्योंकि स्ट्रिंग 'नमस्ते' में चार अक्षर होते हैं।

## गहराई से जानकारी:

पावरशेल की .Length property का उपयोग करना स्ट्रिंग की लंबाई जानने का एक सीधा और सरल तरीका है। स्ट्रिंग प्रिंसिपल्स की पहचान और मापन प्रोग्रामिंग का महत्वपूर्ण हिस्सा है और यह कन्सेप्ट अधिकांश प्रोग्रामिंग भाषाओं में समान होती है।

	Size में भी शायद लंबाई का एक विकल्प हो सकता है, लेकिन .Length से इसे तुलना करने पर यह अधिक विशिष्ट नहीं होता।

## और देखें:

1. [https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-comparisons?view=powershell-7.1](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-comparisons?view=powershell-7.1)
2. [https://www.tutorialspoint.com/powershell/powershell_strings.htm](https://www.tutorialspoint.com/powershell/powershell_strings.htm)
3. [https://www.learntek.org/blog/powershell-string-manipulation/](https://www.learntek.org/blog/powershell-string-manipulation/)

ध्यान देने वाला बिन्दु यह है कि पावरशेल PowerShell 7.1 वर्जन पर आधारित हैं।