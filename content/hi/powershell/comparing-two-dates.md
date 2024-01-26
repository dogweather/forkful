---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:34:16.670796-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना का मतलब है दो तारीखों को आपस में तुलना करना। प्रोग्रामर इसे तय करने के लिए करते हैं कि क्या कोई घटना पहले हुई थी, बाद में, या एक ही समय पर।

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
