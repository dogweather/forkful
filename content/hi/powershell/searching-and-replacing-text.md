---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

*सर्च और रिप्लेस* टेक्स्ट की एक प्रक्रिया है जिसमें आप किसी विशिष्ट शब्द या वाक्यांश को विशिष्ट पाठ में खोजने की कोशिश करते हैं, और फिर उसको किसी अन्य शब्द या वाक्यांश से बदलते हैं। प्रोग्रामर्स इसका प्रयोग विशिष्ट बग्स को ठीक करने, डेटा का स्थानांतरण, और उनके कोड को अद्यतित रखने के लिए करते हैं।

## कैसे करें:

इसे करने के लिए PowerShell में `replace` ऑपरेटर का उपयोग किया जा सकता है।

```PowerShell
$text = "Hello, world!"
$newText = $text -replace "world", "PowerShell"
echo $newText
```

इस उदाहरण में, "world" को "PowerShell" के साथ बदल दिया जाता है, जिससे आउटपुट "Hello, PowerShell!" होता है।

## गहरी डाइव:

1. *ऐतिहासिक संदर्भ*: टेक्स्ट सर्च और रिप्लेस ऑपरेशन का उपयोग प्राचीन कंप्यूटर सिस्टम्स में भी होता था, लेकिन PowerShell में इसका उपयोग और भी सुगम और शक्तिशाली हो गया है।

2. *विकल्प*: `replace` के अलावा, PowerShell में भी `regex` (रेगुलर एक्सप्रेशन) में सर्च और रिप्लेस की सुविधाएं हैं। 

3. *कार्यान्वयन विवरण*: `replace` ऑपरेटर का उपयोग करके, PowerShell पहले वे सभी अवस्थान खोजता है जहां हमारा सर्च टेक्स्ट मिलता है, और फिर उन्हें रिप्लेसमेंट टेक्स्ट से बदल देता है।

## देखने के लिए:

1. [PowerShell का ऑफिशियल डॉक्युमेंटेशन](https://docs.microsoft.com/en-us/powershell/)
2. [PowerShell की सर्च और रिप्लेस ट्यूटोरियल](https://www.tutorialspoint.com/powershell/powershell_regular_expressions.htm)
3. [StackOverflow पर PowerShell के सम्बंधित प्रश्न](https://stackoverflow.com/questions/tagged/powershell)