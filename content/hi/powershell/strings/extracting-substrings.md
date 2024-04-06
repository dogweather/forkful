---
date: 2024-01-20 17:46:55.677575-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) PowerShell\
  \ \u092E\u0947\u0902 \u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `.Substring()`\
  \ \u092E\u0947\u0925\u0921 \u0914\u0930 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0911\u092A\u0930\u0947\u091F\u0930 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964."
lastmod: '2024-04-05T22:38:53.570667-06:00'
model: gpt-4-1106-preview
summary: ") PowerShell \u092E\u0947\u0902 \u0938\u092C\u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `.Substring()` \u092E\u0947\u0925\u0921 \u0914\u0930 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u0911\u092A\u0930\u0947\u091F\u0930 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
