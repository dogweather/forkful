---
date: 2024-01-20 17:34:16.670796-07:00
description: "\u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\
  \u0932\u0928\u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0926\u094B\
  \ \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0906\u092A\u0938 \u092E\
  \u0947\u0902 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0924\
  \u092F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u0915\u094B\u0908\
  \ \u0918\u091F\u0928\u093E \u092A\u0939\u0932\u0947 \u0939\u0941\u0908 \u0925\u0940\
  , \u092C\u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u090F\u0915 \u0939\u0940\
  \ \u0938\u092E\u092F\u2026"
lastmod: '2024-03-13T22:44:52.724036-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0926\u094B \u0924\
  \u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0906\u092A\u0938 \u092E\u0947\
  \u0902 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0924\u092F\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u0915\u094B\u0908 \u0918\
  \u091F\u0928\u093E \u092A\u0939\u0932\u0947 \u0939\u0941\u0908 \u0925\u0940, \u092C\
  \u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u090F\u0915 \u0939\u0940 \u0938\u092E\
  \u092F \u092A\u0930\u0964."
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
