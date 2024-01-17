---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "PowerShell: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों है?
पावरशेल (PowerShell) में 'स्ट्रिंग की लंबाई' ढूंढना क्या है? इसका उपयोग वे प्रोग्रामर क्यों करते हैं?
यह एक सरल कार्य है जो प्रोग्रामिंग में अधिकतर उपयोग किया जाता है। इसका मुख्य उद्देश्य होता है स्ट्रिंग के अक्षरों की संख्या निकालना।

## कैसे करें:
पावरशेल में स्ट्रिंग की लंबाई निकालने के लिए आपको निम्न आदेश को प्रयोग करना होगा:

```PowerShell
$str = "हिंदी भाषा"
$str.Length
```

आपको परिणाम में अक्षरों की संख्या का फार्मैट मिलेगा। इसका उपयोग किसी भी स्ट्रिंग की लंबाई जानने के लिए किया जा सकता है। आप एक स्ट्रिंग की लंबाई के साथ-साथ निम्न विकल्पों को भी प्रयोग कर सकते हैं:

```PowerShell
$str = "विश्लेषक"
$str.ToCharArray().Count
$str.Split("श").Count-1
```

## गहराई में जाएँ:
इतिहास, विकल्प और स्क्रिप्टिंग तकनीकों की दृष्टि से हम पावरशेल में स्ट्रिंग की लंबाई निकालने का उपयोग कर सकते हैं। यह भी संभव है कि यह कार्य अन्य भाषाओं में भी किया जा सकता है, जैसे C# या एक्सेल वर्कबुक। आप नील गांजे (https://neelbaggam.com) की अपनी वेबसाइट पर PowerShell से जुड़े अन्य रोचक विषयों की जानकारी प्राप्त कर सकते हैं।

## अन्य संबंधित स्रोत:
(1) https://www.techgig.com/tech-news/What-is-the-Finding-Length-of-a-String-function-in-PowerShell-220214 (2) https://technet.microsoft.com/en-us/library/ee692803.aspx (3) https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings (4) https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netframework-4.8