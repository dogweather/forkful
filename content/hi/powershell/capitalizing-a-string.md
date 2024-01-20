---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "PowerShell: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

शीर्षक: PowerShell में स्ट्रिंग्स की मूंटनी कैसे करें?

## क्या और क्यों?
अक्षरों की मूंटनी करना का मतलब है कि स्ट्रिंग में हर शब्द के पहले अक्षर को बड़ा (capital) करना। कोडों में इसका इस्तेमाल तब किया जाता है, जब हमें उपयोगकर्ता की इनपुट को किसी निर्धारित फॉर्मैट में दर्ज करना होती है। 

## कैसे करें:
जी हाँ, यह उत्कृष्ट है। PowerShell में, हम `ToTitleCase` मेथड का उपयोग करके यह कर सकते हैं। यहां एक उदाहरण है:

```PowerShell
[string] $myString = "powershell में स्ट्रिंग्स की मूंटनी"
$textInfo = [Globalization.CultureInfo]::CurrentCulture.TextInfo
[string] $capitalizedString = $textInfo.ToTitleCase($myString)
Write-Output $capitalizedString
```
सैंपल आउटपुट:

```PowerShell
Powershell में स्ट्रिंग्स की मूंटनी 
```

## गहराई में:
`ToTitleCase` मेथड का उपयोग करना एक पुराना और प्रभावी तरीका है स्ट्रिंग की मूंटनी करने का। इसके विकल्प आपकी भाषा या प्लेटफार्म पर निर्भर कर सकते हैं। PowerShell में, आप इसे पाइप ट्रिक का उपयोग करके आसानी से लागू कर सकते हैं। `ToTitleCase` मेथड `Globalization.CultureInfo`  क्लास का हिस्सा है, जो `.NET`  का हिस्सा है। 

## अधिक जानकारी:
यदि आप PowerShell के कठिन समस्याओं का समाधान करने में रुचि रखते हैं, तो निम्नलिखित लिंक आपके लिए उपयोगी हो सकते हैं:

1. [PowerShell स्ट्रिंग्स ट्यूटोरियल](https://docs.microsoft.com/hi-in/powershell/scripting/developer/text/string-type?view=powershell-7.1): हाउटूज और उदाहरणों के साथ विभिन्न स्ट्रिंग ऑपरेशन्स।
2. [.NET डॉक्यूमेंटेशन](https://docs.microsoft.com/hi-in/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0): `ToTitleCase` मेथड का विस्तृत विवरण।
3. [कन्वर्जन और कास्टिंग](https://www.sapien.com/blog/2014/09/10/powershell-casting-and-conversion/): PowerShell में टाइप कन्वर्जन कैसे किया जाता है।