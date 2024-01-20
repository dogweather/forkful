---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक नई परियोजना का आरंभ करना यानी एक नया इजीक्यूटेबल कोड बनाना शुरू करना है। प्रोग्रामर्स इसे नई संकेतिक समस्या का समाधान करने या एक नयें कार्यक्षेत्र में अपनी क्षमता का प्रदर्शन करने के लिए करते हैं।

## कैसे करें:

पावरशेल में एक नई 'Hello World' स्क्रिप्ट बनाने का उदाहरण नीचे दिया गया है:

```PowerShell
# नई स्क्रिप्ट।
"Hello, World!" | Out-File -FilePath ./Hello.ps1

# स्क्रिप्ट चलाएं।
.\Hello.ps1
```

उपर्युक्त कोड का उत्पादन निम्न होगा:

```PowerShell
Hello, World!
```

## गहराई में:

(1) ऐतिहासिक प्रसंग:
PowerShell, Microsoft द्वारा 2006 में जारी हुआ, इसे Windows सिस्टम प्रशासन कार्यों से निपटने के लिए बनाया गया था। इसके बाद से, यह उच्च क्षमता वाली स्क्रिप्टिंग भाषा में विकसित हुई है।

(2) विकल्प:
PowerShell के अलावा, Python, Java, C++, आदि. भी नई परियोजनाओं के लिए उपयोग की जा सकती हैं।

(3) क्रियान्वयन विवरण:
PowerShell स्थितिपात्रों (variables), फ़ंक्शन्स, और CMDlets (built-in कमांड) का उपयोग करके प्रवाहों को संचालित करता है, जो Windows के मूल आपरेटिंग सिस्टमतांत्रिक क्षमताओं से बाहर जा सकते हैं।

## देखें भी:

1. [PowerShell डॉक्यूमेंटाशन](https://docs.microsoft.com/en-us/powershell/)
2. [PowerShell स्क्रिप्टिंग गाइड](https://docs.microsoft.com/en-us/powershell/scripting/how-to-use-docs?view=powershell-7.1)
3. [PowerShell के बारे में विकीपीडिया](https://en.wikipedia.org/wiki/PowerShell)