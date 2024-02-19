---
aliases:
- /hi/powershell/interpolating-a-string/
date: 2024-01-20 17:52:16.418249-07:00
description: "\u0907\u0902\u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u090F\u0915\
  \ \u0910\u0938\u0940 \u0924\u0915\u0928\u0940\u0915 \u0939\u0948 \u091C\u093F\u0938\
  \u0938\u0947 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u0947 \u092C\u0940\
  \u091A \u092E\u0947\u0902 \u0935\u0948\u0930\u093F\u090F\u092C\u0932\u094D\u0938\
  \ \u0915\u094B \u0921\u093E\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0907\u0938\u0938\u0947 \u0915\u094B\u0921 \u0938\u093E\u092B-\u0938\u0941\u0925\
  \u0930\u093E \u0914\u0930 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u092A\u0922\
  \u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092C\u0928\u0924\u093E \u0939\
  \u0948\u0964"
lastmod: 2024-02-18 23:09:03.708562
model: gpt-4-1106-preview
summary: "\u0907\u0902\u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u090F\u0915\
  \ \u0910\u0938\u0940 \u0924\u0915\u0928\u0940\u0915 \u0939\u0948 \u091C\u093F\u0938\
  \u0938\u0947 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u0947 \u092C\u0940\
  \u091A \u092E\u0947\u0902 \u0935\u0948\u0930\u093F\u090F\u092C\u0932\u094D\u0938\
  \ \u0915\u094B \u0921\u093E\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0907\u0938\u0938\u0947 \u0915\u094B\u0921 \u0938\u093E\u092B-\u0938\u0941\u0925\
  \u0930\u093E \u0914\u0930 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u092A\u0922\
  \u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092C\u0928\u0924\u093E \u0939\
  \u0948\u0964"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
इंटरपोलेशन एक ऐसी तकनीक है जिससे टेक्स्ट के बीच में वैरिएबल्स को डाल सकते हैं। इससे कोड साफ-सुथरा और आसानी से पढ़ने योग्य बनता है।

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
