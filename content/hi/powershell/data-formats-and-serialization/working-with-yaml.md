---
title:                "YAML के साथ काम करना"
aliases:
- hi/powershell/working-with-yaml.md
date:                  2024-02-03T19:27:02.629868-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, जिसे YAML Ain't Markup Language कहा जाता है, एक मानव-पठनीय डेटा सीरियलाइज़ेशन भाषा है। प्रोग्रामर इसे अक्सर कॉन्फ़िगरेशन फाइलों और भाषाओं के बीच डेटा ट्रांसमिशन के लिए उपयोग करते हैं। इसकी सादगी और पठनीयता इसे विशेष रूप से उन कार्यों के लिए लोकप्रिय बनाती है जिनमें पर्यावरण, अनुप्रयोगों, या सेवाओं को सेट अप करना शामिल है जहां कॉन्फ़िगरेशन महत्वपूर्ण होते हैं और इसे आसानी से समझा और संपादित किया जाना चाहिए।

## कैसे:
PowerShell, डिफॉल्ट रूप से, YAML पार्सिंग के लिए कोई बिल्ट-इन cmdlet के साथ नहीं आता, लेकिन जब आप `powershell-yaml` मॉड्यूल का उपयोग करते हैं या `ConvertFrom-Json` के साथ युग्मित किसी टूल जैसे `yq` का उपयोग करके YAML को PowerShell ऑब्जेक्ट में बदलते हैं, तो यह YAML के साथ सहजता से काम करता है।

### `powershell-yaml` मोड्यूल का उपयोग करना:
पहले, मोड्यूल को इंस्टॉल करें:
```PowerShell
Install-Module -Name powershell-yaml
```

एक YAML फाइल को पढ़ें:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

एक PowerShell ऑब्जेक्ट को YAML फाइल में लिखें:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

`output.yml` का उदाहरण:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### `yq` और `ConvertFrom-Json` के साथ YAML पार्सिंग:
एक अन्य दृष्टिकोण `yq` के उपयोग में शामिल है, जो एक हल्का और पोर्टेबल कमांड-लाइन YAML प्रोसेसर है। `yq` YAML को JSON में बदल सकता है, जिसे PowerShell मूल रूप से पार्स कर सकता है।

पहले, सुनिश्चित करें कि `yq` आपके सिस्टम पर इंस्टॉल है।
फिर चलाएँ:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

यह विधि विशेष रूप से उन उपयोगकर्ताओं के लिए उपयोगी है जो क्रॉस-प्लेटफ़ॉर्म वातावरणों में काम करते हैं या PowerShell के भीतर JSON का उपयोग करना पसंद करते हैं।
