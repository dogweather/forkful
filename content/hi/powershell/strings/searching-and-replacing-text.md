---
date: 2024-01-20 17:58:57.552360-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.673924-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```PowerShell
# फाइल से टेक्स्ट सर्च और रिप्लेस
(Get-Content 'example.txt').Replace('पुराना', 'नया') | Set-Content 'example.txt'

# परिणाम को चेक करना
Get-Content 'example.txt'
```
यह `example.txt` फाइल में हर `पुराना` शब्द को `नया` के साथ बदल देगा।

```PowerShell
# रेगेक्स का इस्तेमाल करके अधिक जटिल सर्च और रिप्लेस
$content = [IO.File]::ReadAllText('example.txt')
$pattern = 'पुराना'
$replacement = 'नया'
$regex = [regex]$pattern
$updatedContent = $regex.Replace($content, $replacement)
[IO.File]::WriteAllText('example.txt', $updatedContent)
```

## Deep Dive (गहराई में जानकारी):
सर्च और रिप्लेस टेक्स्ट की कला पुरानी है। पहले, लोग टाइपराइटर पर भी गलतियों को सही करने के लिए इसी तरह की क्रियाएँ करते थे। कंप्यूटरों के आने से, इसे एडिटर्स और स्क्रिप्टिंग लैंग्वेजेज ने बहुत आसान बना दिया। जैसे PowerShell में `.Replace()` और `[regex]` का इस्तेमाल। इसके अल्टरनेटिव हैं, जैसे की `sed` लिनक्स पर और `findstr` विंडोज़ CMD में। अवधारणा एक ही है, लेकिन इम्प्लीमेंटेशन अलग-अलग संभावनाएँ प्रस्तुत करती हैं।

## See Also (इसे भी देखें):
- [PowerShell स्क्रिप्टिंग गाइड](https://docs.microsoft.com/en-us/powershell/scripting/)
- [Regex सपोर्ट इन PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
