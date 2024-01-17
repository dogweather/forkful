---
title:                "रेग्युलर एक्सप्रेशन का प्रयोग"
html_title:           "PowerShell: रेग्युलर एक्सप्रेशन का प्रयोग"
simple_title:         "रेग्युलर एक्सप्रेशन का प्रयोग"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रेगुलर एक्सप्रेशन का उपयोग करके, हम पाठ डेटा में ढूँढने और फिल्टर करने में सहायता प्राप्त कर सकते हैं। प्रोग्रामर्स इसका उपयोग करते हैं क्योंकि यह उन्हें और अधिक सक्रिय और तेज़ ढंग से डेटा को प्रोसेस करने की अनुमति देता है।

## कैसे करें?

```PowerShell
$input = "मेरा नाम जॉन है। मैं अमेरिका से हूं।"
$input -match "जॉन" # Output: जॉन
```

इस उदाहरण में, हमने `$input` चरित्र स्ट्रिंग के साथ मैच करने के लिए `-match` ऑपरेटर का उपयोग किया है। हमारे द्वारा दिए गए रेगुलर एक्सप्रेशन `"जॉन"` के साथ मैच कुछ भी है जो `$input` में शामिल होगा, और हमें उसका आउटपुट मिलेगा।

```PowerShell
$files = Get-ChildItem "C:\Users\John\Documents" -Recurse # Output: फ़ोल्डर में सभी फ़ाइलें और उनके सबफ़ोल्डर
$files | Where-Object { $_.Name -match ".txt" } # Output: सभी .txt फ़ाइलें
```

इस उदाहरण में, हमने फ़ोल्डर से सभी फ़ाइलें (`Get-ChildItem`) प्राप्त की हैं और फिर उनमें से केवल `.txt` फ़ाइलें फ़िल्टर करने के लिए `Where-Object` लॉजिकल ऑपरेटर का उपयोग किया है।

## गहराई में जाएं

रीज़ुलर एक्सप्रेशन्स का उद्योग काफी बार से किया गया है। पुराने संगणक आधारित डेटा प्रोसेस करने की रीज़ुलर एक्सप्रेशन से समस्या थी, लेकिन मॉडर्न डेटा भंडारण समाधानों ने यह समस्या दूर कर दिया। अन्य विकल्पों की तुलना में, रीज़ुलर एक्सप्रेशन्स सबसे सक्रिय और तेज़ तरीके हैं डेटा में खोज और फ़िल्टर करने के लिए।

रीज़ुलर एक्सप्रेशन्स को बड़े डेटा सेट पर काम करने के लिए अनुकूलित किया गया है, और इस स्पेशल उदाहरण में हमने इसका उपयोग एक ऑनलाइन नेटवर्क स्कैन टूल के रूप में देखा है।

## और भी देखें

- [PowerShell Regular Expression अनुप्रयोग दस्तावेज़। पृष्ठ 98](https://www.manning.com/books/learn-windows-powershell-in-a-month-of-lunches-third-edition)
- [Learn Regular Expressions tutorial](https://www.regular-expressions.info/tutorial.html)