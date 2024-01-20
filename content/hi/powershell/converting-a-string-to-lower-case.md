---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

मामला क्या है? - स्ट्रिंग को लोअर केस (निम्न मामले) में बदलने का मतलब है, जिसे हम प्रोग्रामिंग में करते हैं। हम यह क्यों करें? - कम्पारिजन के लिए। ध्यान दें, कंप्यूटर 'A' और 'a' को अलग मानते हैं, इसलिए हम अक्सर सभी अक्षरों को लोअरकेस में बदलते हैं, ताकि हम उन्हें सही तरीके से तुलना कर सकें।

## कैसे (How to):

आप PowerShell में स्ट्रिंग को निम्नमामले में बदल सकते हैं जैसे कि नीचे दिखाया गया है:

```PowerShell
$string = "Hello, PowerShell"
$lowerCaseString = $string.ToLower()
Write-Host $lowerCaseString
```
इसे चलाने पर, आपको निम्नलिखित आउटपुट मिलेगी:

```PowerShell
hello, powershell
```

## गहरा अध्ययन (Deep Dive):

PowerShell स्ट्रिंग्स के लिए `.ToLower()` विधि का उपयोग करने का इतिहास भले ही नया हो, लेकिन यह जावास्क्रिप्ट और पायथन जैसी भाषाओं से प्रभावित है। वैकल्पिक विधियाँ `.ToLowerInvariant()` और `.ToLower(culture)` हैं, जो विशेष कल्चर के साथ-साथ इंटरनेशनलाइजेशन के मुद्दों को भी समाधान कर सकती हैं। आपने देखा होगा कि `.ToLower()` मेमोरी में नया लोअर केस स्ट्रिंग क्रिएट करता है और किसी भी मूल स्ट्रिंग को बदलता नहीं है जो इसकी प्रोग्रामिंग व्यवहार में एक महत्वपूर्ण तत्व है। 

## यह भी देखें (See Also):

- [Microsoft PowerShell Documentation in Hindi](https://docs.microsoft.com/hi-in/powershell/)