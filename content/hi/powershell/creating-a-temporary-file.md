---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अस्थायी फ़ाइल बनाना मान्यता प्राप्त एक फ़ाइल होती है जिसे केवल कुछ समय के लिए उपयोग किया जाता है। प्रोग्रामर इसे डाटा की अस्थायी जांच, त्रुटि निवारण, या फ़ाइल अंतरण के दौरान उपयोग करते हैं। 

## कैसे?

PowerShell में, आप `GetTempFileName()` मॉड्यूल का उपयोग करके एक अस्थायी फ़ाइल बना सकते हैं। 

```PowerShell 
$tempFile = [IO.Path]::GetTempFileName()
```

इस उदाहरण में, `$tempFile` अस्थायी फ़ाइल के पथ को स्टोर करेगा। 

## गहन विचार 

अस्थायी फ़ाइलों का इतिहास उन दिनों से है जब कंप्यूटर कम स्थान और संसाधनों के साथ चल रहे थे। यह तत्काल डाटा संग्रहण और संगठन के लिए एक उपयोगी प्रक्रिया थी। `GetTempFileName()` PowerShell इसे आसानी से पोर्ट करने और चलाने के लिए स्पष्ट, ईमानदार और कुशल तरीका प्रदान करता है।

वैकल्पिक रूप से, अन्य भाषाओं जैसे कि Python या Java में भी इसे किया जा सकता है, लेकिन PowerShell विंडोज परिवेश में ऐसा करने का सबसे सीधा और आसान तरीका है।

## देखें भी

अधिक जानकारी के लिए, इन स्रोतों का परीक्षण करें: 
1. PowerShell डॉक्स: https://docs.microsoft.com/powershell/
2. अस्थायी फ़ाइल प्रबंधन: https://www.ibm.com/docs/en/zos/2.4.0?topic=files-using-temporary-data-set-management