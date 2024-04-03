---
date: 2024-01-20 17:37:58.937007-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?): PowerShell\
  \ \u092E\u0947\u0902, `Get-Date` \u0915\u092E\u093E\u0902\u0921\u0932\u0947\u091F\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0906\
  \u0938\u093E\u0928\u0940 \u0938\u0947 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\
  \u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:52.722408-06:00'
model: gpt-4-1106-preview
summary: "PowerShell \u092E\u0947\u0902, `Get-Date` \u0915\u092E\u093E\u0902\u0921\
  \u0932\u0947\u091F \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \u0915\u0947 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0924\u093E\u0930\u0940\
  \u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\
  \u0902 \u092C\u0926\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to (कैसे करें?):
PowerShell में, `Get-Date` कमांडलेट इस्तेमाल करके आसानी से तारीख को स्ट्रिंग में बदल सकते हैं।

```PowerShell
# वर्तमान तारीख और समय को स्ट्रिंग में बदलना
$dateString = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
$dateString
```
आउटपुट:
```
2023-03-15 14:23:45
```

एक विशिष्ट फॉर्मेट में:
```PowerShell
# एक कस्टम फॉर्मेट में तारीख को स्ट्रिंग में बदलना
$customDateString = (Get-Date).ToString("dd MMM, yyyy")
$customDateString
```
आउटपुट:
```
15 मार्च, 2023
```

## Deep Dive (गहराई में जानकारी):
PowerShell में डेटटाइम को स्ट्रिंग में बदलने की क्षमता .NET क्लास लाइब्रेरी पर आधारित है। इतिहास में, कमांडलेट और डॉटनेट मेथड्स डेटा फॉर्मेटिंग के लिए विकसित किए गए थे। `Get-Date -Format` एक सरल तरीका है, जबकि `[datetime]::Now.ToString()` जैसे विधियाँ जटिल फॉर्मेटिंग के लिए हैं। I18N (Internationalization) के माध्यम से, तारीखों को स्थानीय संस्कृतियों के अनुसार फॉर्मेट किया जा सकता है, जैसे की हिंदी में दिन-महीना-वर्ष।

## See Also (इसे भी देखें):
- PowerShell की आधिकारिक डॉक्यूमेंटेशन: [Get-Date Command](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1) 
- तारीख और समय के फॉर्मेट: [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
