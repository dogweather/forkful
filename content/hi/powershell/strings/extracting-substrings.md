---
title:                "सबस्ट्रिंग्स निकालना"
aliases: - /hi/powershell/extracting-substrings.md
date:                  2024-01-20T17:46:55.677575-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
सबस्ट्रिंग निकालना मूल रूप से स्ट्रिंग का छोटा हिस्सा प्राप्त करने की प्रक्रिया है। प्रोग्रामर विशेष डेटा को प्रोसेस, एनालिसिस करने और जरूरत के मुताबिक डेटा को पुनर्व्यवस्थित करने के लिए सबस्ट्रिंग निकालते हैं।

## कैसे करें? (How to:)
PowerShell में सबस्ट्रिंग निकालने के लिए `.Substring()` मेथड और स्ट्रिंग ऑपरेटर का इस्तेमाल कर सकते हैं। 

```PowerShell
# .Substring() का उपयोग
$stringExample = "नमस्ते PowerShell"
$substring = $stringExample.Substring(0, 6) 
$substring
```

उदाहरण से आउटपुट: `नमस्ते`

```PowerShell
# स्ट्रिंग ऑपरेटर का उपयोग
$substring = $stringExample[0..5] -join '' 
$substring
```

उपरोक्त दोनों उदाहरणों से आउटपुट समान होगा।

## गहन जानकारी (Deep Dive)
सबस्ट्रिंग निकालने की जरूरत तब पड़ी जब प्रोग्रामिंग में बड़ी स्ट्रिंग्स को संभालने की जरूरत आई। पुराने समय में, सीमित संसाधनों के कारण, केवल जरूरी डेटा का इस्तेमाल होता था और ऐसे में सबस्ट्रिंग निकालना अहम हो जाता था।

PowerShell में सबस्ट्रिंग की तकनीकों का विकल्प भी हैं, जैसे कि `-match` ऑपरेटर, रेगुलर एक्सप्रेशन (Regex) और `.Split()` मेथड शामिल हैं। ये तरीके और भी जटिल पैटर्न्स को हैंडल कर सकते हैं।

```PowerShell
# Regex का उपयोग
$regexPattern = 'स्ते'
if ($stringExample -match $regexPattern) {
    $matches[0]
}
```

जब सबस्ट्रिंग की बात आती है, तो प्रदर्शन और स्पीड पर विचार करना महत्वपूर्ण होता है। बड़ी स्ट्रिंग्स में `.Substring()` प्रायः तेज होता है।

## संबंधित लिंक्स (See Also)
- [Microsoft Official PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [PowerShell Gallery for Scripts, Modules and DSC Resources](https://www.powershellgallery.com/)
