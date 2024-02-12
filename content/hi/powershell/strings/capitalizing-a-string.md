---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases: - /hi/powershell/capitalizing-a-string.md
date:                  2024-02-03T19:06:34.801118-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
PowerShell में एक स्ट्रिंग को कैपिटलाइज़ करना इसका मतलब है दिए गए स्ट्रिंग के पहले अक्षर को ऊपरी केस (बड़ा अक्षर) में बदलना, जबकि बाकी स्ट्रिंग को अपरिवर्तित रखना। प्रोग्रामर अक्सर फॉर्मेटिंग के उद्देश्यों के लिए इस कार्य को करते हैं, जैसे कि यूज़र इंटरफेस में प्रदर्शित करने के लिए टेक्स्ट तैयार करना या जनरेटेड दस्तावेज़ों में व्याकरणिक नियमों का पालन करना।

## कैसे करें:
PowerShell, एक बहुउद्देश्यीय उपकरण होने के नाते, आपको तृतीय-पक्ष पुस्तकालयों की आवश्यकता के बिना सीधे तरीके से एक स्ट्रिंग को कैपिटलाइज़ करने की अनुमति देता है। आप यह कैसे कर सकते हैं यह यहां है:

```powershell
# CultureInfo से .Net का निर्मित method 'ToTitleCase' का उपयोग करना
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
आउटपुट:
```
Hello world
```

नोट: यह विधि प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज़ करती है। अगर आप सख्ती से केवल स्ट्रिंग के पहले अक्षर को ही कैपिटलाइज़ करना चाहते हैं और बाकी को जैसा है वैसा ही रखना चाहते हैं, तो आप इस तरह कुछ कर सकते हैं:

```powershell
# केवल स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ करना
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
आउटपुट:
```
Hello world
```

PowerShell सीधे तौर पर केवल स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ करने के लिए एक सरल फ़ंक्शन शामिल नहीं करता है, लेकिन `Substring(0,1).ToUpper()` और संयोजन जैसे मूल स्ट्रिंग मैनीपुलेशन तरीकों को जोड़कर, हम आसानी से वांछित परिणाम प्राप्त कर सकते हैं।
