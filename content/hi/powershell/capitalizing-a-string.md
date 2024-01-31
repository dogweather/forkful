---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)
**क्या है**: स्ट्रिंग कैपिटलाइज़ेशन का मतलब होता है हर शब्द के पहले अक्षर को बड़ा (कैपिटल) करना।
**क्यों**: प्रोग्रामर्स अकसर टेक्स्ट को औपचारिकता देने या नाम और शीर्षक को स्पष्टता से प्रदर्शित करने के लिए स्ट्रिंग कैपिटलाइज़ करते हैं।

## How to: (कैसे करें?)
PowerShell में, स्ट्रिंग्स को कैपिटलाइज़ करने के लिए आप `ToTitleCase` मेथड का उपयोग कर सकते हैं:

```PowerShell
# ग्लोबलाइज़ेशन क्लास लोड करें
Add-Type -AssemblyName System.Globalization

# कल्चर इन्फो ऑब्जेक्ट तैयार करें
$textInfo = [Globalization.CultureInfo]::CurrentCulture.TextInfo

# स्ट्रिंग कैपिटलाइज़ करें
$capitalizedString = $textInfo.ToTitleCase("yeh ek udaharan hai")
$capitalizedString
```
नतीजा:
```
Yeh Ek Udaharan Hai
```

## Deep Dive (गहराई से जानकारी)
PowerShell स्ट्रिंग मेथड `ToTitleCase` .NET क्लास `TextInfo` से आता है, जो `System.Globalization` नेमस्पेस का हिस्सा है। यह प्रकार्य पहली बार .NET Framework में शामिल किया गया था। आज भी, यह मेथड प्रोग्रामर्स को स्ट्रिंग्स के हर शब्द के पहले अक्षर को बड़ा करने की अनुमति देता है।

विकल्प: PowerShell में डायरेक्ट स्ट्रिंग मेथड `ToUpper()` और `ToLower()` भी हैं, जो क्रमश: पूरे स्ट्रिंग को कैपिटल या स्मॉल केस में बदल देते हैं। लेकिन `ToTitleCase` प्रत्येक शब्द के लिए कैपिटलाइज़ेशन प्रदान करता है, जो हेडिंग्स या टाइटल्स में उपयोगी होता है।

इम्प्लीमेंटेशन डीटेल्स: `ToTitleCase` को इस्तेमाल करते वक्त, यह ध्यान देना जरुरी है कि यह मेथड शब्दों की सामान्य सीमा पर ध्यान देता है। यदि स्ट्रिंग पहले से ही अप्पर केस में हैं, तो इसे प्रत्येक शब्द की पहचान नहीं हो पाएगी। इसलिए इसे स्ट्रिंग्स के साथ उपयोग से पहले उसे `ToLower()` या `ToLowerCase()` से छोटा करना उत्तम होगा।

## See Also (और भी जानकारी)
- [Microsoft Docs on TextInfo](
  https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo)
- [Microsoft Docs on ToTitleCase](
  https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
- [PowerShell string manipulation](
  https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-strings?view=powershell-7.1)
