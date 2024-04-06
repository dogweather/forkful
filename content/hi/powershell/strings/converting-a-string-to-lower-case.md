---
date: 2024-01-20 17:39:43.342011-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell\
  \ \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B\
  \ lower case \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `.ToLower()` \u092E\u0947\u0925\u0921 \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0906\u092E \u0939\u0948\u0964 \u092F\u0939 \u092E\
  \u0947\u0925\u0921 .NET Framework \u0938\u0947 \u0906\u0924\u093E \u0939\u0948 \u0914\
  \u0930 \u0907\u0938\u0947\u2026"
lastmod: '2024-04-05T22:51:07.349040-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell \u092E\u0947\
  \u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B lower case\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `.ToLower()` \u092E\u0947\u0925\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0906\u092E \u0939\u0948\u0964 \u092F\u0939 \u092E\u0947\u0925\
  \u0921 .NET Framework \u0938\u0947 \u0906\u0924\u093E \u0939\u0948 \u0914\u0930\
  \ \u0907\u0938\u0947 \u0938\u092D\u0940 \u092A\u093E\u0935\u0930\u0936\u0947\u0932\
  \ \u0935\u0930\u094D\u091C\u0928\u094D\u0938 \u092E\u0947\u0902 \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\
  \u0924\u093E \u0939\u0948\u0964 \u0939\u093F\u0938\u094D\u091F\u094B\u0930\u093F\
  \u0915\u0932 \u0915\u0949\u0928\u094D\u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u092E\u0947\u0902, \u0907\u0938 \u0924\u0930\u0939 \u0915\u0947 \u0915\u0928\u094D\
  \u0935\u0930\u094D\u091C\u0928 \u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u092A\
  \u0939\u0932\u0940 \u092C\u093E\u0930 \u0924\u092C \u0906\u0908 \u091C\u092C \u092F\
  \u0942\u091C\u0930\u094D\u0938 \u0938\u0947 \u092E\u093F\u0932\u0928\u0947 \u0935\
  \u093E\u0932\u0947 \u0907\u0928\u092A\u0941\u091F \u0915\u094B \u0938\u093E\u092E\
  \u093E\u0928\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u0940 \u0928\u0940\u0921\
  \ \u0939\u0941\u0908\u0964 Alternatives \u0915\u0947 \u0924\u094C\u0930 \u092A\u0930\
  , `ToLowerInvariant()` \u0915\u093E \u092F\u0942\u091C\u093C \u0915\u0930\u0928\u093E\
  \ \u092D\u0940 \u092E\u0941\u092E\u0915\u093F\u0928 \u0939\u0948, \u091C\u094B \u0915\
  \u0932\u094D\u091A\u0930-\u0907\u0928\u0938\u0947\u0902\u0938\u093F\u091F\u093F\u0935\
  \ \u0915\u0928\u094D\u0935\u0930\u094D\u091C\u0928 \u0926\u0947\u0924\u093E \u0939\
  \u0948\u0964 PowerShell Core \u092E\u0947\u0902 `ToLowerInvariant()` \u0905\u0915\
  \u094D\u0938\u0930 \u092A\u094D\u0930\u0947\u092B\u0930 \u0915\u093F\u092F\u093E\
  \ \u091C\u093E\u0924\u093E \u0939\u0948, \u0915\u094D\u092F\u094B\u0902\u0915\u093F\
  \ \u092F\u0939 \u0915\u094D\u0930\u0949\u0938-\u092A\u094D\u0932\u0947\u091F\u092B\
  \u0949\u0930\u094D\u092E \u0939\u0948\u0964 \u092A\u0930\u092B\u0949\u0930\u094D\
  \u092E\u0947\u0902\u0938 \u0915\u0947 \u0932\u093F\u0939\u093E\u091C \u0938\u0947\
  , `.ToLower()` \u0914\u0930 `.ToLowerInvariant()` \u0926\u094B\u0928\u094B\u0902\
  \ \u0939\u0940 \u092B\u093E\u0938\u094D\u091F \u0939\u0948\u0902 \u0914\u0930 \u0932\
  \u093E\u0930\u094D\u091C \u0921\u0947\u091F\u093E \u092A\u0930 \u092D\u0940 \u0905\
  \u091A\u094D\u091B\u0947 \u0930\u093F\u091C\u0932\u094D\u091F\u094D\u0938 \u0926\
  \u0947\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

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
