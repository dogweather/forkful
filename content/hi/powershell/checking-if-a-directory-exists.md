---
title:                "डायरेक्टरी मौजूद है या नहीं यह जांचना"
html_title:           "Go: डायरेक्टरी मौजूद है या नहीं यह जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं यह जांचना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# डायरेक्टरी मौजूद है या नहीं, कैसे जांचें? PowerShell का प्रयोग करना सीखें।

## क्या और क्यों?
डायरेक्टरी की मौजूदगी की जांच करने का अर्थ है, यह सुनिश्चित करना कि एक विशेष डायरेक्टरी कंप्यूटर पर मौजूद है या नहीं। प्रोग्रामर्स इसे ताकि कॉड या स्क्रिप्ट अनपेक्षित त्रुटियों से बच सके, इसका उपयोग करते हैं।

## कैसे करें:
नीचे दिया गया कोड सैंपल दिखाता है कि कैसे हम PowerShell का उपयोग करके डायरेक्टरी की मौजूदगी की जांच कर सकते हैं।

```PowerShell
$path = "C:\Temp\MyDirectory"

if (Test-Path $path) {
    Write-Host "Directory exists"
} else {
    Write-Host "Directory does not exist"
}
```

कोड जब चलता है। यह `C:\Temp\MyDirectory` पथ मौजूद है या नहीं, जांचेगा। अगर पथ मौजूद होता है, तो यह "Directory exists" प्रिंट करेगा, अन्यथा "Directory does not exist" प्रिंट होगा। 

## गहराई में जाने:
विगत में, प्रोग्रामर्स `if (dir $path)` की तरह पुराने तरीकों का उपयोग करके डायरेक्टरी की मौजूदगी की जांच करते थे। अब PowerShell `Test-Path` cmdlet का प्रदान करता है, जो इसे और अधिक सुविधाजनक और विश्वसनीय बनाता है। 

इसके विकल्पों में Python, Bash और अन्य स्क्रिप्टिंग भाषाओं में विभिन्न उपयोगों के लिए विभिन्न तरीके होते हैं, लेकिन PowerShell का उपयोग Windows पर विधानानुसार काम करने के लिए बनाया गया है।

## यह भी देखें:
1. [PowerShell का आधिकारिक दस्तावेजीकरण](https://docs.microsoft.com/en-us/powershell/)
2. [PowerShell स्क्रिप्टिंग गाइड](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
3. [PowerShell `Test-Path` cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)