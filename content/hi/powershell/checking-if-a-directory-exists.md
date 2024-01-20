---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:58:25.590091-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व चेक करना मतलब है पता लगाना कि कोई फोल्डर सिस्टम में है भी या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि गलतियों को रोक सकें और फाइल्स को सेव या रीड करने से पहले सही जगह का पता चल सके।

## How to: (कैसे करें:)
```PowerShell
# डायरेक्टरी चेक करना:
$directoryPath = "C:\ExamplePath"

# Test-Path cmdlet का उपयोग करके चेक करें
if (Test-Path $directoryPath) {
    Write-Host "डायरेक्टरी मौजूद है!"
} else {
    Write-Host "डायरेक्टरी मौजूद नहीं है!"
}

# सैंपल आउटपुट:
# अगर मौजूद है, तो:
"डायरेक्टरी मौजूद है!"

# अगर नहीं मौजूद है, तो:
"डायरेक्टरी मौजूद नहीं है!"
```

## Deep Dive (गहराई से जानकारी):
डायरेक्टरी चेक करने के लिए `Test-Path` PowerShell कमांडलेट काफी पुराना और भरोसेमंद तरीका है। यह न सिर्फ फोल्डर के लिए बल्कि फाइल्स के लिए भी इस्तेमाल होता है। अल्टरनेटिव में, कुछ स्क्रिप्टर्स `[System.IO.Directory]::Exists($path)` डॉटनेट क्लास का उपयोग भी करते हैं, लेकिन `Test-Path` ज्यादा स्ट्रेटफॉरवर्ड है। इसे अलग अलग पैरामीटर्स जैसे कि `-PathType` के साथ और भी शक्तिशाली बना सकते हैं, जिससे आप चेक कर सकते हैं कि ऑब्जेक्ट एक डायरेक्टरी है, फाइल है, या कुछ और।

## See Also (देखने के लिए):
- [Test-Path Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.2)
- [about_Automatic_Variables (इसमें `$PWD` जो वर्तमान डायरेक्टरी को दिखाता है)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.2)
- [PowerShell Scripting guide](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.2)
- [File System Provider](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/providers/filesystem-provider?view=powershell-7.2)