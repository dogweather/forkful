---
title:                "Regular Expressions का प्रयोग"
html_title:           "Bash: Regular Expressions का प्रयोग"
simple_title:         "Regular Expressions का प्रयोग"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
बाश में नियमित अभिव्यक्तियों का उपयोग करने से, हम टेक्स्ट के एक निर्दिष्ट पैटर्न को खोज और प्रोसेस कर सकते हैं। प्रोग्रामर्स इसका उपयोग करते हैं ताकि कोडिंग में प्राप्त होने वाली जटिलताओं को सुलझा सकें और कोड को अधिक अपशिष्ट बनाने से बच सकें।

## कैसे करे:
```Bash
# उदाहरण 1: सादे अंक की पहचान
echo "अंक संख्या है या नहीं?" | grep -E [0-9] #Output: अंक संख्या है या नहीं?
```
```Bash
# उदाहरण 2: ईमेल पते की जांच
email="hindi@programmer.com"
echo $email | grep -E ^[a-zA-Z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$ # Output: hindi@programmer
```

## गहराई में जाएं:
(1) निर्देशांक भाषाएं को खजाने के लिए सुपर्चार और अन्य विकल्पों के लिए इतिहासी संदर्भ, (2) निर्माण लक्ष्यों को पूरा करने के लिए स्वयं से शुरू करना, और (3) नियमित अभिव्यक्तियों का उपयोग करते हुए विविध प्रोग्रामिंग क्षेत्रों में उपयोग करने के बारे में विस्तृत जानकारी। 

## और भी देखें:
- [बाश में नियमित अभिव्यक्तियों का आपने उपयोग किया है।](https://www.shellscript.sh/regex.html)
- [बाश में फ़ंक्शन का उपयोग करना सीखे।](https://ryanstutorials.net/bash-scripting-tutorial/bash-functions.php)
- [ नियमित अभिव्यक्तियों के लिए और विस्तृत संबंधित स्रोत।](https://www.regular-expressions.info/)