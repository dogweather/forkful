---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:02.629868-07:00
description: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0939\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935\
  -\u092A\u0920\u0928\u0940\u092F \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\
  \u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u092D\u093E\u0937\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938\u0947 \u0905\u0915\u094D\u0938\u0930 \u0915\u0949\u0928\u094D\u092B\
  \u093C\u093F\u0917\u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902\
  \ \u0914\u0930 \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u092C\u0940\u091A\
  \u2026"
lastmod: 2024-02-19 22:05:11.760595
model: gpt-4-0125-preview
summary: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0939\u093E\
  \ \u091C\u093E\u0924\u093E \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935-\u092A\
  \u0920\u0928\u0940\u092F \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\u092F\
  \u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u092D\u093E\u0937\u093E \u0939\
  \u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\
  \u0938\u0947 \u0905\u0915\u094D\u0938\u0930 \u0915\u0949\u0928\u094D\u092B\u093C\
  \u093F\u0917\u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902 \u0914\
  \u0930 \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u092C\u0940\u091A\u2026"
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
