---
title:                "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
html_title:           "PowerShell: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी भी प्रोग्रामर की जिंदगी में डेटा को संशोधित करने का एक अधिकृत तरीका है मानचित्र के अनुरूप वर्णों को हटा देना। वे अपने कोड को साफ, सुगम और सेहतमंद रखने के लिए इस विधि का प्रयोग करते हैं।

## कैसे करें:
```PowerShell
# एक डिरेक्टरी से वर्ण हटाएं
Get-ChildItem | Rename-Item -NewName {$_.Name -replace "वर्ण", ""}
```

```PowerShell
# अपने कुल वर्णों से गिनती में कमी करें
$path = "C:\Users\Username\Documents\Sample.txt"
$content = Get-Content $path
$updatedContent = $content -replace "वर्ण", ""
$content | Measure-Object -Character
$updatedContent | Measure-Object -Character
```

## गहरा खुराक:
डेटा को संशोधित करने के मूल तरीकों में शामिल है, वर्ण-प्रतिसादित लाइन डालना, और टेक्स्ट स्ट्रिंग को निर्दिष्ट पैटर्न से छानना। शुरुआती प्रोग्रामरों के लिए, यह भाषा मुश्किल और समझने में कठिन हो सकती है, लेकिन इसका अभ्यास असली में खुशी का कारण बन सकता है।

## और भी देखें:
- [Regular Expressions in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- [Using the Replace Operator in PowerShell](https://devblogs.microsoft.com/scripting/use-the-replace-operator-in-powershell/)