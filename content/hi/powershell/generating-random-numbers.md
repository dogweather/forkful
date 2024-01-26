---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:19.199103-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
रैंडम नंबर्स बनाना मतलब है ऐसे नंबर्स उत्पन्न करना जिनका क्रम अनिश्चित हो। प्रोग्रामर इसे डेटा एनालिसिस, गेमिंग, सुरक्षा, और टेस्टिंग में उपयोग करते हैं जहां निश्चित न पैटर्न चाहिए होता है।

## कैसे करें? (How to:)
```PowerShell
# एक रैंडम नंबर जेनरेट करना
$randomNumber = Get-Random
Write-Output $randomNumber

# एक निश्चित श्रेणी में रैंडम नंबर जेनरेट करना
$randomInRange = Get-Random -Minimum 10 -Maximum 50
Write-Output $randomInRange

# एक लिस्ट से रैंडम एलीमेंट पिक करना
$colors = 'Red', 'Green', 'Blue', 'Yellow'
$randomColor = Get-Random -InputObject $colors
Write-Output $randomColor
```
सैंपल आउटपुट:
```
165324234
23
Green
```

## विस्तार से (Deep Dive)
रैंडम नंबर जेनरेटर (RNG) का इतिहास कंप्यूटर साइंस में गहराई से जुड़ा है। शुरू में, रैंडमनेस को मापने के लिए फिजिकल तरीकों (जैसे पासा फेंकना) का इस्तेमाल होता था। आज, हम pseudo-random number generators (PRNGs) का प्रयोग करते हैं, जो मैथेमेटिकल फॉर्मुलास से रैंडम लगने वाले नंबर्स बनाते हैं। PowerShell में `Get-Random` cmdlet एक PRNG प्रदान करता है।

'Get-Random' का इम्प्लीमेंटेशन `System.Random` क्लास से .NET Framework में किया गया है, जो एक डिटर्मिनिस्टिक तरीके से रैंडम वैल्यूज उत्पन्न करता है। यानी कि, अगर आप एक ही 'seed' वैल्यू देते हैं, तो आपको बार-बार समान क्रम में नंबर्स मिलेंगे। इसके अलावा, PowerShell का स्क्रिप्टिंग इंजन इसे और भी अनुकूल बनाता है।

## संदर्भ (See Also)
- Microsoft का आधिकारिक डॉक्युमेंटेशन [Get-Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
- रैंडमनेस के कांसेप्ट पर [.NET क्लास लाइब्रेरी](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- प्रोग्रामिंग और न्यूमेरिकल एनालिसिस में रैंडम नंबर्स [का महत्त्व](https://en.wikipedia.org/wiki/Random_number_generation)
