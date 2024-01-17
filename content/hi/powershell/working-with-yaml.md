---
title:                "Yaml से काम करना"
html_title:           "PowerShell: Yaml से काम करना"
simple_title:         "Yaml से काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML का उपयोग करना क्या है और आखिरकार प्रोग्रामर्स इसे क्यों करते हैं? YAML (YAML Ain't Markup Language) एक आसान और इंटरैक्टिव स्ट्रक्चर डेटा स्टोर करने के लिए है जो कि मेमोरी में होती है और प्रोग्रामर्स को कंप्यूटर या सर्वर के भीतर डेटा को ऑपरेट करने की अनुमति देती है।

## कैसे :

```PowerShell
# डेटा को फाइल में स्टोर करें
@'
Powershell:
 एक:
    स्ट्रिंगों का एक ग्रुप।
 दो:
    पूर्वावलोकन हेतु बूलीयन
 तीन:
    सूचीकरण हेतु एक
'@ | Out-File sample.yml

# डेटा को खोलें
$ymlFile = Get-Content sample.yml
$ymlData = $ymlFile | ConvertFrom-Yaml

# डेटा को मूल्यों के साथ अपडेट करें
$ymlData.Powershell.एक = "एक मेथड या फंक्शन"
$ymlData.Powershell.तीन = @("हिंदी", "अंग्रेज़ी", "स्पेनिश")

# अपडेटेड डेटा को फाइल में लिखें
$ymlData | ConvertTo-Yaml | Out-File sample.yml

# डेटा प्रिंट करें
$ymlData
```

आउटपुट:
```PowerShell
Powershell
-----------
@{एक=एक मेथड या फंक्शन; दो=पूर्वावलोकन हेतु बूलीयन; तीन=System.Collections.Generic.List

## गहराई:
YAML का इतिहास भी बेहद रोचक है। इसे 2001 में बनाया गया था और इसे पहले प्रोग्रामर्स आमतौर पर ऑप्शन या प्रोपर्टी की तरह उपयोग करते थे। लेकिन वर्तमान में YAML फाइलों और डेटा प्रोसेसिंग के लिए यूटिलिटी के रूप में त्योहार बन गया है। YAML की तुलना में इसके अन्य विकल्प जैसे XML और JSON में जादूगर हैं। YAML का एक और बड़ा लाभ यह है कि यह सामान्य टेक्स्ट के साथ आसानी से रखा जा सकता है और हमेशा से सुव्यवस्थित उत्पादन डेटा को ले कर जाता है।

## देखें भी:
- [ऑफिशियल YAML दस्तावेज़ीकरण](https://yaml.org/spec/1.2/spec.html)
- [XML और JSON की तुलना में YAML](https://stackify.com/json-vs-xml/)
- [PowerShell में ConvertFrom-Yaml और ConvertTo-Yaml का उपयोग](https://mcpmag.com/articles/2016/07/12/working-with-yaml-files.aspx)