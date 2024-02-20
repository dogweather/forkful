---
date: 2024-01-20 17:39:43.342011-07:00
description: "String \u0915\u094B lower case \u092E\u0947\u0902 \u092C\u0926\u0932\
  \u0928\u093E \u092E\u0924\u0932\u092C \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u0915\u094B \u091B\u094B\u091F\u093E \u0915\u0930\u0928\u093E\u0964 Programmers\
  \ \u0907\u0938\u0947 \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u092C case sensitivity \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923\
  \ \u0939\u094B, \u091C\u0948\u0938\u0947 \u092F\u0942\u091C\u0930 \u0907\u0928\u092A\
  \u0941\u091F \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u093E \u092F\u093E\
  \u2026"
lastmod: 2024-02-19 22:05:11.697029
model: gpt-4-1106-preview
summary: "String \u0915\u094B lower case \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\
  \u094B \u091B\u094B\u091F\u093E \u0915\u0930\u0928\u093E\u0964 Programmers \u0907\
  \u0938\u0947 \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u092C\
  \ case sensitivity \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923\
  \ \u0939\u094B, \u091C\u0948\u0938\u0947 \u092F\u0942\u091C\u0930 \u0907\u0928\u092A\
  \u0941\u091F \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u093E \u092F\u093E\
  \u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String को lower case में बदलना मतलब अक्षरों को छोटा करना। Programmers इसे तब करते हैं जब case sensitivity महत्वपूर्ण हो, जैसे यूजर इनपुट को साफ करना या डेटा की consistency बनाए रखना।

## How to: (कैसे करें:)
```PowerShell
# एक सिंपल स्ट्रिंग को Lower Case में कन्वर्ट करना
$string = "HELLO World"
$lowerCaseString = $string.ToLower()
Write-Output $lowerCaseString
```
Output:
```
hello world
```

```PowerShell
# एक एरे में सभी स्ट्रिंग्स को Lower Case में बदलना
$stringsArray = "FIRST", "Second", "THIRD"
$lowerCaseArray = $stringsArray | ForEach-Object { $_.ToLower() }
$lowerCaseArray
```
Output:
```
first
second
third
```

## Deep Dive (विस्तार से जानकारी)
PowerShell में स्ट्रिंग को lower case में बदलने के लिए `.ToLower()` मेथड का इस्तेमाल आम है। यह मेथड .NET Framework से आता है और इसे सभी पावरशेल वर्जन्स में इस्तेमाल किया जा सकता है। हिस्टोरिकल कॉन्टेक्स्ट में, इस तरह के कन्वर्जन की जरूरत पहली बार तब आई जब यूजर्स से मिलने वाले इनपुट को सामान्य करने की नीड हुई। 

Alternatives के तौर पर, `ToLowerInvariant()` का यूज़ करना भी मुमकिन है, जो कल्चर-इनसेंसिटिव कन्वर्जन देता है। PowerShell Core में `ToLowerInvariant()` अक्सर प्रेफर किया जाता है, क्योंकि यह क्रॉस-प्लेटफॉर्म है। 

परफॉर्मेंस के लिहाज से, `.ToLower()` और `.ToLowerInvariant()` दोनों ही फास्ट हैं और लार्ज डेटा पर भी अच्छे रिजल्ट्स देते हैं। 

## See Also (अधिक जानकारी के लिए)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [.NET String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- [.NET String.ToLowerInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-6.0)
