---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
date:                  2024-01-26T03:43:37.382707-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
PowerShell में एक स्ट्रिंग से उद्धरण चिन्हों को हटाना आपके पाठ के चारों ओर लगे एकल (`'`) या दोहरे (`"`) उद्धरण चिन्ह हटा देता है। प्रोग्रामरों को अक्सर प्रोसेसिंग, तुलना, या आउटपुट उद्देश्यों के लिए स्ट्रिंग्स को साफ करने की आवश्यकता होती है, विशेषकर जब उपयोगकर्ता इनपुट या फ़ाइल पार्सिंग से निपट रहे हों।

## कैसे करें:
आप एक स्ट्रिंग से उद्धरण चिन्हों को हटाने के लिए `-replace` ऑपरेटर का उपयोग कर सकते हैं। यहाँ पर कैसे है:

```PowerShell
# एकल उद्धरण चिन्हों को बदलें
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # आउटपुट: Hello, World!

# दोहरे उद्धरण चिन्हों के बदलें
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # आउटपुट: Hello, World!
```

दोनों प्रकारों के लिए:

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # रेगेक्स चरित्र वर्ग का उपयोग ध्यान दें
Write-Output $cleanString  # आउटपुट: Hi there, she said.
```

कंसोल से नमूना आउटपुट कुछ इस तरह दिखेगा:

```
Hello, World!
Hello, World!
Hi there, she said.
```

## गहराई में जाना
Microsoft की आँखों में PowerShell का टिमटिमाना शुरू होने से पहले, Windows में टेक्स्ट प्रोसेसिंग अक्सर बैच स्क्रिप्ट्स के दायरे में थी जिनकी सीमित क्षमताएं थीं। PowerShell की पेशकश के साथ शक्तिशाली स्ट्रिंग मैनिपुलेशन सुविधाएँ आईं जिससे स्क्रिप्टिंग बहुत अधिक रोबस्ट बन गई।

`-replace` के विकल्प मौजूद हैं, जैसे कि स्ट्रिंग के शुरू और अंत में उद्धरण चिन्हों को हटाने के लिए `.Trim()` मेथड का उपयोग करना, लेकिन वे समान नियंत्रण या रेगेक्स सपोर्ट प्रदान नहीं करते।

```PowerShell
# शुरू और अंत में उद्धरण के लिए .Trim() का उपयोग करना
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # आउटपुट: Hello, World!
```

ध्यान दें, `-replace` पीछे से रेगेक्स का उपयोग करता है, इसलिए जब आप इसके साथ काम कर रहे होते हैं, विशेष चरित्रों को एस्केप करने की आवश्यकता होती है अगर आप उन्हें लक्षित कर रहे हैं। यदि आपको उद्धरण हटाने पर अधिक ग्रेनुलर नियंत्रण की आवश्यकता है, `-replace` के साथ रेगेक्स में गहराई से डाइव करना जाने का तरीका है, जो आपको अत्यधिक लचीलापन देता है।

## भी देखें
- PowerShell में रेगेक्स के बारे में अधिक जानने के लिए, ऑफिशल डॉक्स चेक करें: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- स्ट्रिंग मैनिपुलेशन की गहराई में जांच करें: [about_String](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- अन्य स्ट्रिंग मेथड्स की खोज करें: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)