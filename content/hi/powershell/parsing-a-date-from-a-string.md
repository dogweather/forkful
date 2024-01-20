---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

पार्सिंग डेट का मतलब होता है एक स्ट्रिंग को डेट टाइप में बदलना। इसका उपयोग तब होता है जब डाटा विशेषकर तारीख, स्ट्रिंग के रूप में प्राप्त होती है और हमें उसे डेट टाइप में प्रविष्ट करना चाहिए। 

## कैसे करें? 

तारीख को किसी स्ट्रिंग से पार्स करने के लिए, PowerShell में [DateTime]::ParseExact फ़ंक्शन का उपयोग कर सकते हैं। यहां कुछ उदाहरण दिए गए हैं। 

```PowerShell  
$dateString = '12-03-2021'
$dateFormat = 'dd-MM-yyyy'
$cultureInfo = [System.Globalization.CultureInfo]::InvariantCulture
$parsedDate = [DateTime]::ParseExact($dateString, $dateFormat, $cultureInfo)
Write-Output $parsedDate
```

आउटपुट होगा:

```PowerShell
Friday, March 12, 2021 12:00:00 AM
```

## गहराई से समझना 

विगत वर्षों में, डेवलपर्स ने विभिन्न तारीख पार्सिंग पद्धतियों का उपयोग किया है, लेकिन PowerShell ने इसे सरल और सुगम बनाया है। वैकल्पिक रूप से हम TryParseExact फ़ंक्शन का भी उपयोग कर सकते हैं जो किसी त्रुटि की स्थिति में फ़ॉल्स लौटाता है, न कि अपवाद उत्पन्न करता है। 

डेट पार्सिंग को सफल बनाने के लिए, सटीक तारीख/समय प्रारूप स्पष्ट रूप से निर्दिष्ट किया जाना चाहिए। 

## और भी देखें 

1. [MSDN - DateTime.ParseExact Method](https://msdn.microsoft.com/en-us/library/w2sa9yss(v=vs.110).aspx)
2. [MSDN - DateTime.TryParseExact Method](https://msdn.microsoft.com/en-us/library/ms131044(v=vs.110).aspx)