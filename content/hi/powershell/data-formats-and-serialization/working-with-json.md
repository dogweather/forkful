---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:04.883165-07:00
description: "\u0915\u0948\u0938\u0947: PowerShell \u092E\u0947\u0902 JSON \u0915\u094B\
  \ \u092A\u0922\u093C\u0928\u0947 \u092F\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A `ConvertFrom-Json`\
  \ cmdlet \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u090F\u0915 JSON \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0926\u093F\u090F \u091C\u093E\u0928\u0947 \u092A\u0930, \u092F\
  \u0939 cmdlet \u0907\u0938\u0947 \u090F\u0915\u2026"
lastmod: '2024-03-13T22:44:52.739208-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u092E\u0947\u0902 JSON \u0915\u094B \u092A\u0922\u093C\u0928\
  \u0947 \u092F\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F, \u0906\u092A `ConvertFrom-Json` cmdlet \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u090F\u0915 JSON \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0926\
  \u093F\u090F \u091C\u093E\u0928\u0947 \u092A\u0930, \u092F\u0939 cmdlet \u0907\u0938\
  \u0947 \u090F\u0915 PowerShell \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932 \u0926\u0947\u0924\u093E \u0939\u0948\u0964\
  ."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे:


### JSON का पार्सिंग
PowerShell में JSON को पढ़ने या पार्स करने के लिए, आप `ConvertFrom-Json` cmdlet का उपयोग कर सकते हैं। एक JSON स्ट्रिंग दिए जाने पर, यह cmdlet इसे एक PowerShell ऑब्जेक्ट में बदल देता है।

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

नमूना आउटपुट:

```
John Doe
```

यह उदाहरण दिखाता है कि कैसे एक सरल JSON स्ट्रिंग को पार्स किया जाए ताकि परिणामस्वरूप ऑब्जेक्ट के गुणों तक पहुँचा जा सके।

### JSON का जेनरेट करना
एक PowerShell ऑब्जेक्ट से JSON उत्पन्न करने के लिए, आप `ConvertTo-Json` cmdlet का उपयोग कर सकते हैं। यह डेटा को एक वेब सर्विस को भेजने या एक कॉन्फ़िगरेशन फ़ाइल में सेव करने के लिए तैयार करने में सहायक है।

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

नमूना आउटपुट:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

यह कोड स्निपेट एक PowerShell ऑब्जेक्ट बनाता है और फिर इसे एक JSON स्ट्रिंग में बदल देता है।
