---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases: - /hi/powershell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:58.937007-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग में बदलना मतलब है डेटा को टेक्स्ट फॉर्मेट में बदलना। इसे प्रोग्रामर्स कॉन्फिगरेशन फाइल्स, लॉग्स, यूजर इंटरफ़ेस में दिखाने और डाटा स्टोरेज के लिए करते हैं।

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
