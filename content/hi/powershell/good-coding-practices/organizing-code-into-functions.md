---
title:                "कोड को फंक्शन्स में व्यवस्थित करना"
aliases:
- /hi/powershell/organizing-code-into-functions/
date:                  2024-01-26T01:11:51.931292-07:00
model:                 gpt-4-1106-preview
simple_title:         "कोड को फंक्शन्स में व्यवस्थित करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड को फ़ंक्शन्स में व्यवस्थित करना का अर्थ है कोड के विशिष्ट भागों को लपेटना और उन्हें एक नाम देना। यह कोड को पुन: उपयोगी, पठनीय और रख-रखाव योग्य बनाने के लिए किया जाता है। एक ही कोड को बार-बार न लिखकर, एक फ़ंक्शन को कॉल करें। समस्या निवारण या अपग्रेड करना चाहते हैं? बिना स्क्रिप्ट के ढेर में से चुनने के, सीधे फ़ंक्शन में सुधार करें।

## कैसे करें:
आइए हम दो संख्याओं के योग की गणना करने के लिए एक फंक्शन लिखें। यह सरल है, लेकिन यह बिंदु को स्पष्ट करता है।

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# फंक्शन को 5 और 10 के साथ कॉल करें
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "योग है $sum"
```

नमूना आउटपुट:

```
योग है 15
```

## गहराई से जानकारी
पावरशेल में फंक्शन्स, जैसे की ज्यादातर भाषाओं में, पुरानी खबर हैं। हम कोड को विभाजित करना फॉरट्रान के दिनों से कर रहे हैं। यह 'पहिए को फिर से नहीं बनाना' के बारे में है। विकल्प? हां, स्क्रिप्ट्स या कमांडलेट्स। लेकिन उनमें स्क्रिप्ट्स के भीतर फंक्शन्स की साफ-सफाई और संदर्भ-संवेदनशीलता का अभाव होता है।

इंप्लीमेंटेशन? फंक्शन्स हमारे उदाहरण की तरह सरल भी हो सकते हैं या फिर स्कोप्स, पाइपलाइन इनपुट और अधिक के साथ जटिल भी। 'एडवांस्ड फंक्शन्स' को लें। वे कमांडलेट्स की नकल करते हैं जिनमें पैरामीटर होते हैं जिनके लक्षण होते हैं, जैसे कि `[Parameter(Mandatory=$true)]`। यह पावरशेल की लचीलापन की एक झलक है।

## देखें भी
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
