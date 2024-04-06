---
date: 2024-01-20 17:34:16.670796-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E \u0915\u0930\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\
  \u0948\u0964 \u0928\u0940\u091A\u0947 \u0926\u093F\u090F \u0917\u090F \u0915\u094B\
  \u0921 \u0915\u094B \u0926\u0947\u0916\u0947\u0902."
lastmod: '2024-04-05T21:53:54.686881-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell \u092E\u0947\
  \u0902 \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u0930\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u0948\u0964\
  \ \u0928\u0940\u091A\u0947 \u0926\u093F\u090F \u0917\u090F \u0915\u094B\u0921 \u0915\
  \u094B \u0926\u0947\u0916\u0947\u0902."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## How to: (कैसे करें:)
PowerShell में तारीखों की तुलना करना आसान है। नीचे दिए गए कोड को देखें:

```PowerShell
# तारीखों का परिभाषित करना
$date1 = Get-Date '1/1/2022'
$date2 = Get-Date '1/1/2023'

# तारीखों की तुलना
if ($date1 -lt $date2) {
    "Date1 is earlier than Date2"
} elseif ($date1 -gt $date2) {
    "Date1 is later than Date2"
} else {
    "Date1 and Date2 are the same"
}
```
सैंपल आउटपुट होगा:
```
Date1 is earlier than Date2
```

## Deep Dive (गहराई में जानकारी:)
PowerShell में तारीखों की तुलना करते समय `-lt` (less than), `-gt` (greater than), `-le` (less than or equal to), `-ge` (greater than or equal to), `-eq` (equal to), और `-ne` (not equal to) ऑपरेटर्स का इस्तेमाल होता है। 

यह कार्यक्षमता .NET क्लास `System.DateTime` पे आधारित है, जिसे PowerShell में एकीकृत किया गया है। इतिहास में तारीखों की तुलना करने के विभिन्न तरीके डेवलप किए गए हैं, पर PowerShell का दृष्टिकोण यह है कि यह डेटा टाइप्स और ऑब्जेक्ट-ओरिएंटेड प्रोग्रामिंग प्रिंसिपल्स का लाभ उठाता है।

वैकल्पिक तौर पर, डेवलपर्स `.Compare`, `.Equals`, जैसे मेथड्स का भी इस्तेमाल कर सकते हैं, और `[datetime]::Compare()` जैसे स्टैटिक मेथड्स भी उपलब्ध हैं।

## See Also (और जानकारी के लिए:)
- [DateTime.CompareTo Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto)
- [System.DateTime संदर्भ](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
