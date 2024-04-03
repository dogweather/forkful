---
date: 2024-01-20 17:42:52.467861-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.672249-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

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
