---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन एक कार्य है जिससे हम वेरिएबल्स की मानों को स्ट्रिंग्स में शामिल कर सकते हैं। प्रोग्रामर्स इसका उपयोग डायनेमिक और पठनीय कोड लिखने के लिए करते हैं।

## कैसे करें:

```Fish Shell
set city "New Delhi"
echo "Welcome to $city!" 
```
आउटपुट:
```
Welcome to New Delhi!
```

## गहराई से समझना:

1. इतिहास: स्ट्रिंग इंटरपोलेशन का उपयोग पुराने इंटरप्रेटेड प्रोग्रामिंग भाषाओं में होता आया है। यह विशेषता Fish shell में भी उपस्थित है।
2. दूसरे विकल्प: आप कर्ली ब्रेसेज `{}` का भी उपयोग कर सकते हैं। जैसे-
```Fish Shell
echo "Welcome to {$city}!" 
```
3. कैसे काम करता है: `$` साइन के इस्तेमाल से बताया जाता है कि वेरिएबल का नाम शुरू हो रहा है, और शेल उस वेरिएबल की मूल्य को खोजता है और उसे स्ट्रिंग में रीप्लेस कर देता है। 

## देखने के लिए भी:

1. एक साधारण ट्यूटोरियल Fish string interpolation के बारे में - [Fish Shell Basic String Interpolation](http://www.fishshell.com/docs/current/tutorial.html)

2. एक विस्तृत गाइड जिसमें सबकुछ है जो आपको Fish Shell के वेरिएबल्स के बारे में पता होना चाहिए - [Fish Shell](http://www.fishshell.com/docs/current/index.html)