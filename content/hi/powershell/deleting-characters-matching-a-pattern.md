---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

मैच होने वाले पैटर्न के कैरेक्टर को डिलीट करना का मतलब है, की हम एक स्ट्रिंग से उस पैटर्न को हटाते हैं जो हमें चाहिए। प्रोग्रामर इसे क्यों करते हैं? उन्हें विचारशीलता से डाटा को संयोजित या मानकीकृत करने के लिए या अनुपयोगी कैरेक्टर्स को हटाने के लिए करना पड़ता है।

## कैसे करें:

PowerShell में, हम `-replace` ऑपरेटर का उपयोग कर ऐसा कर सकते हैं। ये एक उदाहरण है:

```PowerShell
$string = "Hello, world!!!"
$newString = $string -replace '[^a-zA-Z0-9 ]', ''
$newString
```

आउटपुट:

```PowerShell
Hello world
```

यहाँ, हमने 'Hello, world!!!' से सभी गैर-अल्फन्यूमेरिक करैक्टर्स को हटाया।

## गहरी डाइव:

1. **Historical context**: powershell `-replace` ऑपरेटर रीगेक्स (Regular expression) का समर्थन करता है, जिसने स्ट्रिंग मेनिपुलेशन को बहुत आसान बना दिया।
2. **Alternatives**: छोटे tasks के लिए, आप `-replace` से दोषपूर्ण कैरेक्टर्स भी हटा सकते हैं, जैसे `$newString = $string -replace ',', '' -replace '!', ''`
3. **Implementaion details**: `-replace` ऑपरेटर को उम्मीद होती है कि पहला पैरामीटर जो दीया गया है वह regexp हो, और दूसरा हो replacement string (जो optional है)। यदि कोई मिलान नहीं मिलता, तो स्ट्रिंग अपरिवर्तित रहता है।

## देखें भी:

- Regular expressions: www.regular-expressions.info/powershell.html
- PowerShell की अधिक जानकारी के लिए: https://docs.microsoft.com/en-us/powershell/
- स्ट्रिंग मेनिपुलेशन के बारे में अधिक जानने के लिए: https://ss64.com/ps/syntax-replace.html