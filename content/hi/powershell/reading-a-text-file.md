---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फ़ाइल पढ़ना यानी उसमें रेखांकित डेटा प्राप्त करना। प्रोग्रामर्स इसे डेटा संसाधन के रूप में उपयोग करते हैं या अन्य प्रोग्रामों के साथ इंटरेक्ट करने के लिए।

## कैसे करें:

### उदाहरण:

```PowerShell 
$content = Get-Content -Path C:\Example\example.txt
Write-Output $content
```

ऊपर के कोड की आउटपुट आपको example.txt फ़ाइल में मौजूद सभी लाइनें प्रदान करेगी।

## गहराई में अन्वेषण:

### ऐतिहासिक प्रकट्य: 

PowerShell में टेक्स्ट फ़ाइलों को पढ़ने की क्षमता Windows PowerShell 1.0 के साथ आई थी। इसका उपयोग डाटा संग्रहण और प्रबंधन में सुविधा के लिए किया जाता है।

### विकल्प:

.NET Framework का StreamReader क्लास भी एक प्रभावी तरीका है फ़ाइलों को पढ़ने का। आप [System.IO.StreamReader]::ReadLine() का उपयोग कर सकते हैं। लेकिन आमतौर पर, Get-Content cmdlet अधिक फ्लेक्सिबल होता है। 

### कार्यान्विति:

Get-Content cmdlet स्ट्रीमिंग तत्व का उपयोग करता है, जिससे यह बड़ी फ़ाइलों के साथ काम करने में समर्थ होता है। यह फ़ाइल को पूरी तरह लोड करने के बजाय चंक्स (टुकड़ों) में डेटा को लोड करता है। 

## और देखें:

1. [Get-Content cmdlet documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
2. [StreamReader Class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)