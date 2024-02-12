---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- /hi/powershell/using-regular-expressions.md
date:                  2024-02-03T19:18:30.662294-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

नियमित व्यंजक (regex) वर्णों की एक श्रृंखला होती है जो एक खोज पैटर्न तैयार करती है, जिसका मुख्य रूप से तार खोजने और संशोधन के लिए उपयोग किया जाता है। कार्यक्रमकर्ता PowerShell में डेटा मान्यता, पार्सिंग, और रूपांतरण जैसे कार्यों के लिए इसका लाभ उठाते हैं, क्योंकि इसमें जटिल पैटर्नों को संभालने में कुशलता और लचीलापन होता है।

## कैसे करें:

PowerShell में, आप `-match`, `-replace`, और `-split` ऑपरेटरों का उपयोग करके, दूसरों के बीच, नियमित व्यंजकों के साथ कार्य कर सकते हैं। आइए कुछ उदाहरणों का अन्वेषण करें:

### पैटर्न से मेल खाते हुए तार की जाँच करने के लिए `-match` का उपयोग
यदि पैटर्न तार में पाया जाता है तो यह ऑपरेटर `$true` लौटाता है, अन्यथा `$false`।

```powershell
"hello world" -match "\w+orld"
# उत्पादन: सच
```

### मिलानों को निकालना
आप स्वतः वैरिएबल `$matches` को एक्सेस करके मिलान मान को निकाल सकते हैं।

```powershell
if ("I have 100 apples" -match "\d+") {
    "संख्या मिली: " + $matches[0]
}
# उत्पादन: संख्या मिली: 100
```

### प्रतिस्थापन के लिए `-replace` का उपयोग
`-replace` ऑपरेटर एक निर्दिष्ट प्रतिस्थापन स्ट्रिंग के साथ पैटर्न के सभी उदाहरणों को बदल देता है।

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# उत्पादन: foo qux qux
```

### `-split` के साथ स्ट्रिंगों को विभाजित करना
एक रेगेक्स पैटर्न के आधार पर एक स्ट्रिंग को सबस्ट्रिंगों की एक सरणी में विभाजित करना।

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# उत्पादन: The quick brown fox jumps
```

### उन्नत पैटर्न मैचिंग
PowerShell `[regex]` क्लास के माध्यम से अधिक जटिल regex कार्य संचालित करने का समर्थन करता है, जो आपको `Matches()`, `Replace()`, और `Split()` जैसे विधियों तक पहुंच प्रदान करता है।

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# उत्पादन: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# उत्पादन: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# उत्पादन: one two three four
```

ये उदाहरण PowerShell में डेटा मैनिपुलेशन और पैटर्न मैचिंग के लिए नियमित व्यंजकों की शक्ति और बहुमुखी प्रतिभा को दर्शाते हैं। Regex का उपयोग करके, कार्यक्रमकर्ता कुशलतापूर्वक जटिल पाठ प्रसंस्करण को प्रदर्शन कर सकते हैं।
