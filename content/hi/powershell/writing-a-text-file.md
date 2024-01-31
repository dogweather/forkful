---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल लिखना मतलब डाटा को टेक्स्ट रूप में सेव करना। प्रोग्रामर इसे कॉन्फिग्यूरेशन, डेटा स्टोरेज, लॉगिंग, और यूज़र डेटा को सहेजने के लिए करते हैं।

## कैसे करें:
```PowerShell
# टेक्स्ट फाइल बनाना और लिखना
"हैलो, यह मेरा टेक्स्ट है!" | Out-File -FilePath .\example.txt

# आउटपुट देखना
Get-Content -Path .\example.txt
```
आउटपुट:
```
हैलो, यह मेरा टेक्स्ट है!
```
```PowerShell
# फाइल में नई लाइन जोड़ना
Add-Content -Path .\example.txt -Value "एक और लाइन!"

# नया आउटपुट देखना 
Get-Content -Path .\example.txt
```
आउटपुट:
```
हैलो, यह मेरा टेक्स्ट है!
एक और लाइन!
```

## गहराई में:

पावरशेल में `Out-File` कमांडलेट का उपयोग शुरू से होता आया है, जिससे फाइल सिस्टम में टेक्स्ट सेव कर सकें। इसे `.NET` क्लासेस के ऊपर बनाया गया है। 'Add-Content', 'Set-Content', और 'Out-File' जैसे कमांडलेट्स हैं जो अलग-अलग तरीकों से फाइल ऑपरेशन्स को हैंडल करते हैं। हालांकि, डाटा खोने के खतरे को रखते हुए, हमेशा `-Append` स्विच का उपयोग करना चाहिए जब हम एक मौजूदा फाइल में डेटा जोड़ रहे हों। 

## संबंधित स्रोत:

- [Microsoft Docs: Out-File](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [Microsoft Docs: Add-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content)
- [Microsoft Docs: Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
