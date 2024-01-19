---
title:                "वेब पेज को डाउनलोड करना"
html_title:           "PowerShell: वेब पेज को डाउनलोड करना"
simple_title:         "वेब पेज को डाउनलोड करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज डाउनलोड करना क्या है और क्यों प्रोग्रामर इसे करते है? वेब पेज डाउनलोड करना एक तरीका है जो आपको इंटरनेट पर उपलब्ध जानकारी तक पहुंचने में मदद करता है। प्रोग्रामर इसे डेटा विश्लेषित करने, वेब साइटों के संरचना और पते पता करने और उपयोगकर्ताओं के लिए वेब पेज को अपडेट करने के लिए करते है।

## कैसे करें:
### उदाहरण 1:
```PowerShell
$url = "https://www.example.com"
$output = Invoke-WebRequest -Uri $url
Write-Host $output
```
### उदाहरण 2:
```PowerShell
Invoke-WebRequest -Uri "https://www.example.com" | Select-Object -ExpandProperty Links | Where-Object {$_.class -eq "nav"} | Select-Object -ExpandProperty href
```

## गहराई में जाएं:
### इतिहासिक पृष्ठभूमि:
वेब पेजों की डाउनलोडिंग के लिए प्रोग्रामरों को पहले से ही कठिनाई से अभिव्यक्ति करने की जरूरत थी। पहले, टेक्स्ट और ग्राफिक्स को सुरक्षित ट्रांसमिशन प्रोटोकॉल के माध्यम से डाउनलोड किया जाता था और फिर उपयोगकर्ताओं को इसका उपयोग करके दिखाया जाता था। लेकिन, Powershell जैसे नए टूल्स ने दुनिया में कुछ स्पष्ट आसानियां दिए हैं जो वेब पेज डाउनलोड को एक स्टेप कर दिया है।

### वैकल्पिक:
कई अन्य पर्याप्त लोग और वेब पेज प्रदाताओं भी वेब पेज को डाउनलोड करने के लिए उपलब्ध हैं। एक उपाय है कि आप सीमित रूप से वेबसाइटों पर से डेटा संग्रहीत कर सकते हैं और उन्हें स्टोर कर सकते हैं, लेकिन यह काफी समय लेगा और उपयुक्त डेटा की संख्या नहीं होगी। दूसरा विकल्प है यूआरएल से अवरोहण करना है जो आपको उपयोगकर्ताओं के लिए बंधक चित्र हो सकता है।

### प्रयोग करने का तरीका:
1. PowerShell को स्टार्ट खोलें।
2. डाउनलोड करने के लिए वेब पेज के यूआरएल को उपयोग करें और निष्पादित करें।
3. ब्राउज़र में वेब पेज के साथ जुड़े लिंक्स का उपयोग करें।

## इसे भी देखें:
- [Microsoft PowerShell ओफ़िशियल वेबसाइट] (https://docs.microsoft.com/en-us/powershell/)
- [वेब पेज की डाउनलोडिंग को समझने के लिए वीडियो मार्गदर्शन] (https://www.youtube.com/watch?v=OzqnxIZlFNo)
- [पूर्ण PowerShell की कस्टमाइजेशन की सूची] (https://github.com/PowerShell/PowerShell)