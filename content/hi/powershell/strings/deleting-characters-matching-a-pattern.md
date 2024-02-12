---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- /hi/powershell/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:52.467861-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/deleting-characters-matching-a-pattern.md"
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
