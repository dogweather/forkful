---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:39:43.342011-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/converting-a-string-to-lower-case.md"
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