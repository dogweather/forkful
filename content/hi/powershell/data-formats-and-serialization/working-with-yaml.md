---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:02.629868-07:00
description: "\u0915\u0948\u0938\u0947: PowerShell, \u0921\u093F\u092B\u0949\u0932\
  \u094D\u091F \u0930\u0942\u092A \u0938\u0947, YAML \u092A\u093E\u0930\u094D\u0938\
  \u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F \u0915\u094B\u0908 \u092C\u093F\
  \u0932\u094D\u091F-\u0907\u0928 cmdlet \u0915\u0947 \u0938\u093E\u0925 \u0928\u0939\
  \u0940\u0902 \u0906\u0924\u093E, \u0932\u0947\u0915\u093F\u0928 \u091C\u092C \u0906\
  \u092A `powershell-yaml` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u092F\
  \u093E\u2026"
lastmod: '2024-04-05T21:53:54.699443-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, \u0921\u093F\u092B\u0949\u0932\u094D\u091F \u0930\u0942\u092A\
  \ \u0938\u0947, YAML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u094B\u0908 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928\
  \ cmdlet \u0915\u0947 \u0938\u093E\u0925 \u0928\u0939\u0940\u0902 \u0906\u0924\u093E\
  , \u0932\u0947\u0915\u093F\u0928 \u091C\u092C \u0906\u092A `powershell-yaml` \u092E\
  \u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u092F\u093E `ConvertFrom-Json` \u0915\
  \u0947 \u0938\u093E\u0925 \u092F\u0941\u0917\u094D\u092E\u093F\u0924 \u0915\u093F\
  \u0938\u0940 \u091F\u0942\u0932 \u091C\u0948\u0938\u0947 `yq` \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 YAML \u0915\u094B PowerShell \u0911\
  \u092C\u094D\u091C\u0947\u0915\u094D\u091F \u092E\u0947\u0902 \u092C\u0926\u0932\
  \u0924\u0947 \u0939\u0948\u0902, \u0924\u094B \u092F\u0939 YAML \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u0939\u091C\u0924\u093E \u0938\u0947 \u0915\u093E\u092E \u0915\
  \u0930\u0924\u093E \u0939\u0948\u0964."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

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
