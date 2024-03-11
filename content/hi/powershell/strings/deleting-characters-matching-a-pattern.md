---
date: 2024-01-20 17:42:52.467861-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932\
  \ \u0916\u093E\u0924\u0947 \u0915\u093F\u0930\u0926\u093E\u0930\u094B\u0902 \u0915\
  \u094B \u0939\u091F\u093E\u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u0941\u091B\
  \ \u0916\u093E\u0938 \u0924\u0930\u0940\u0915\u0947 \u0915\u0947 \u0915\u0948\u0930\
  \u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u091C\u094B \u090F\u0915 \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u093E \u0905\u0928\u0941\u0938\u0930\u0923 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902, \u0909\u0928\u094D\u0939\u0947\u0902 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 \u0928\u093F\
  \u0915\u093E\u0932 \u0926\u0947\u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\
  \u0964\u2026"
lastmod: '2024-03-11T00:14:26.603009-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0915\u093F\u0930\u0926\u093E\u0930\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u0941\u091B \u0916\
  \u093E\u0938 \u0924\u0930\u0940\u0915\u0947 \u0915\u0947 \u0915\u0948\u0930\u0947\
  \u0915\u094D\u091F\u0930\u094D\u0938 \u091C\u094B \u090F\u0915 \u092A\u0948\u091F\
  \u0930\u094D\u0928 \u0915\u093E \u0905\u0928\u0941\u0938\u0930\u0923 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902, \u0909\u0928\u094D\u0939\u0947\u0902 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 \u0928\u093F\u0915\
  \u093E\u0932 \u0926\u0947\u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  \u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
पैटर्न से मेल खाते किरदारों को हटाना यानी कुछ खास तरीके के कैरेक्टर्स जो एक पैटर्न का अनुसरण करते हैं, उन्हें स्ट्रिंग्स से निकाल देना होता है। प्रोग्रामर्स जब डेटा साफ करना चाहते हैं या जरूरी डेटा को परिष्कृत करना चाहते हैं, तब वे ऐसा करते हैं।

## कैसे करें:
```PowerShell
# उदाहरण: '`' के साथ शुरू होने वाले किरदार हटाना
$exampleString = "`this is` a `test`"
$cleanString = $exampleString -replace '`.+?`', ''
Write-Output $cleanString
# Output: " is a "
```
```PowerShell
# संख्याओं को हटा रहे हैं
$numberString = "There are 123 numbers in this 456 sentence."
$cleanNumberString = $numberString -replace '[0-9]+', ''
Write-Output $cleanNumberString
# Output: "There are  numbers in this  sentence."
```

## गहराई से जानकारी:
इतिहास के पन्नों पर जाएं, तो पैटर्न मैचिंग और स्ट्रिंग मॉडिफिकेशन की शुरुआत शुरुआती प्रोग्रामिंग भाषाओं के साथ ही हुई थी। PowerShell में `-replace` ऑपरेटर Regex (Regular Expressions) का उपयोग करके पैटर्न मैचिंग प्रदान करता है। इसके अलावा, .NET क्लासेस जैसे `[regex]` का प्रयोग करके भी स्ट्रिंग्स से डेटा हटाया जा सकता है, जो ज्यादा उन्नत खोज और मॉडिफिकेशन क्षमताएं देता है। PowerShell में `-replace` का इस्तेमाल जल्दी और सरलता से स्ट्रिंग्स में बदलाव के लिए किया जाता है, विशेषकर जब सीधे सीधे पैटर्न्स का मामला हो।

## संबंधित सूत्र:
- PowerShell Regex गाइड: https://ss64.com/ps/syntax-regex.html
- Microsoft डॉक्यूमेंटेशन `-replace` ऑपरेटर: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators
- Regex Quick Reference: https://www.regular-expressions.info/refquick.html
