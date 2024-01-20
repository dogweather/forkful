---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

सबस्ट्रिंग निकालना यानी एक बड़ी स्ट्रिंग से छोटी स्ट्रिंग को अलग करना। कार्यक्रमकर्ताएं इसे उपयोगी जानकारी को अलग करने के लिए करते हैं, जैसे कि एक फ़ाइल का नाम या एक URL का एक हिस्सा ।

## कैसे :

आप PowerShell में substring निकालने के लिए `.substring()` function का उपयोग करते हैं। यहां उदाहरण है:

```powershell
$str = "Hello, World!"
$sub_str = $str.Substring(0, 5)
echo $sub_str
```

उपरोक्त उदाहरण में, `$sub_str` का परिणाम `"Hello"` होगा।

## गहराई में :

PowerShell .substring() method का उपयोग करके हम substring को प्राप्त कर सकते हैं। इसे जानकारी में से विशिष्ट विवरण निकालने के लिए इस्तेमाल किया जाता है। जैसे कि, आप एक ईमेल पते में से डोमेन नाम निकालने के लिए इसका उपयोग कर सकते हैं। 

इसके विकल्प के रूप में, आप `-split`, `-replace` jesi cmdlets का उपन्यास कर सकते हैं, लेकिन वे generally regular expressions का उपयोग करती हैं जो अधिक complex हो सकते हैं।

## और देखें :

1. [PowerShell Substring Method(https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_methods?view=powershell-7.1#substringint-int)]
2. [PowerShell Tutorials - Substring Extraction(https://www.tutorialspoint.com/powershell/powershell_string_manipulation.htm)]

अब आप substring को कैसे निकाले, इसका गहरयां और इंतजाम जानते हैं। अगली बार जब आपको कोई जानकारी निकालने की आवश्यकता हो, तो फ़ंक्शन का उपयोग करें।