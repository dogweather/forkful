---
date: 2024-01-20 17:52:16.418249-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0909\
  \u091F\u092A\u0941\u091F: \u0928\u092E\u0938\u094D\u0924\u0947, \u0928\u0947\u0939\
  \u093E!."
lastmod: '2024-04-05T21:53:54.642844-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0909\u091F\u092A\
  \u0941\u091F."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
```PowerShell
$name = "नेहा"
$greeting = "नमस्ते, $name!"
Write-Host $greeting
```
आउटपुट: नमस्ते, नेहा!

```PowerShell
$daysUntilDeadline = 3
$reminder = "तुम्हारे पास केवल $daysUntilDeadline दिन बचे हैं।"
Write-Host $reminder
```
आउटपुट: तुम्हारे पास केवल 3 दिन बचे हैं।

## Deep Dive (गहराई से जानकारी)
स्ट्रिंग इंटरपोलेशन का इस्तेमाल शुरू से प्रोग्रामिंग में हो रहा है। PowerShell में स्ट्रिंग इंटरपोलेशन के लिए डबल कोट्स (" ") का उपयोग किया जाता है। सिंगल कोट्स (' ') को यूज करने पर वैरिएबल्स एक्सपांड नहीं होते हैं। PowerShell में हाल के वर्ज़न्स में `$using:` स्कोप मॉडिफायर जोड़ा गया है जिससे remote sessions के दौरान लोकल वेरिएबल्स को इंटरपोलेट कर सकते हैं।

विकल्प के रूप में, आप `-f` ऑपरेटर का उपयोग करके भी फॉर्मेटेड स्ट्रिंग्स बना सकते हैं:

```PowerShell
$name = "राज"
$greeting = "नमस्ते, {0}" -f $name
Write-Host $greeting
```

लेकिन इंटरपोलेशन ज़्यादा सरल और बेहतर माना जाता है। PowerShell स्क्रिप्ट लिखते समय परफॉरमेंस और पठनीयता में संतुलन बनाना महत्वपूर्ण है, और इंटरपोलेशन दोनों को साधता है।

## See Also (और जानकारी के लिए)
- [about_Quoting_Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [about_Scopes](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_scopes)
- [about_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators)
