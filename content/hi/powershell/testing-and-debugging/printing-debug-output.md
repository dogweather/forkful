---
title:                "डीबग आउटपुट प्रिंट करना"
aliases:
- /hi/powershell/printing-debug-output.md
date:                  2024-01-20T17:53:52.601184-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डीबग आउटपुट प्रिंट करना यानी विकास के दौरान कोड से संबंधित जानकारी का आउटपुट दिखाना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे समझ सकें कि कोड कैसे चल रहा है और संभावित समस्याओं का पता लगा सकें।

## कैसे करें:
PowerShell में डीबग आउटपुट प्रिंट करने के लिए आम तौर पर `Write-Host`, `Write-Debug`, और `Write-Verbose` जैसे cmdlets का उपयोग किया जाता है।

```PowerShell
# सामान्य प्रिंटिंग
Write-Host "नमस्ते! यह संदेश है।"

# डिबग संदेश
Write-Debug "डिबग: इसे केवल तब दिखाई देगा जब डिबग पसंदीदा हो।"

# वर्बोज संदेश
Write-Verbose "वर्बोज: इसे केवल तब दिखाई देगा जब वर्बोज पसंदीदा हो।"
```

```PowerShell
# $DebugPreference और $VerbosePreference सेटिंग
$DebugPreference = 'Continue'
$VerbosePreference = 'Continue'

Write-Host "नमस्ते! यह संदेश है।"
Write-Debug "अब यह डिबग संदेश दिखेगा।"
Write-Verbose "और यह वर्बोज संदेश भी दिखेगा।"
```
## गहराई से जानकारी:
PowerShell में डीबग आउटपुट एक महत्वपूर्ण फंक्शन है, जिसकी जड़ें पहले के स्क्रिप्टिंग और प्रोग्रामिंग भाषाओं में नजर आती हैं। `Write-Host` से आउटपुट सीधे कंसोल में प्रिंट होता है, पर `Write-Debug` और `Write-Verbose` अधिक लचीलापन प्रदान करते हैं, क्योंकि उन्हें पसंदीदा सेटिंग्स के आधार पर टॉगल किया जा सकता है। इन cmdlets का उपयोग करने से स्क्रिप्ट्स को डिबग करना आसान हो जाता है और कोड के विभिन्न हिस्सों की निगरानी के लिए सूक्ष्म नियंत्रण मिलता है।

## संबंधित स्रोत:
- [About Write-Debug](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug)
- [About Write-Host](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-host)
- [About Write-Verbose](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-verbose)
