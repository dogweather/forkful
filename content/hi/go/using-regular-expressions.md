---
title:                "नियमित अभिव्यक्तियों का प्रयोग करना"
html_title:           "Go: नियमित अभिव्यक्तियों का प्रयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का प्रयोग करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
ऑनलाइन सर्च या टेक्स्ट प्रोसेसिंग के दौरान आपने शायद शब्द जोड़ने या निकालने को एक ही वाक्य में संक्षिप्त और प्रभावी ढंग से करने के लिए विशेष तरीके से धुंध ही होंगे। यही चीज़ रेगुलर एक्सप्रेशन हमारे लिए कर सकते हैं। प्रोग्रामर्स इसका इस्तेमाल करते हैं ताकि वे स्ट्रिंग पैटर्न को ढूंढ़ सकें जो उनके कोड में समान हो। 

## कैसे करें:
``` Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // एक सामान्य रेगुलर एक्सप्रेशन का उपयोग करके स्ट्रिंग के साथ खेलें।
    
    // इस स्ट्रिंग अंतरिक्ष में "Gopher" ढूंढ़ें।
    match, _ := regexp.MatchString("Gopher", "Welcome to the Golang World!")
    fmt.Println(match) // true
    
    // स्थान पर रिप्लेस करने के लिए "Gophers" से "Golang" पर रिप्लेस करें।
    re := regexp.MustCompile("Gophers")
    fmt.Println(re.ReplaceAllString("I love Gophers!", "Golang")) // I love Golang!
}
```

## गहराई में जाएं:
रेगुलर एक्सप्रेशन को 1950 लोकनाथ जी के समय से लगभग ७० साल पहले डेवलप किया गया था। निकटतम [वैकल्पिक](https://en.wikipedia.org/wiki/Regular_expression#Comparison_of_regular_expression_engines) में शामिल भाषाएं पुराने ग्रीक मौलिक शंख रूपी दो बिंदुओं का उपयोग करती हैं। और कई भाषा [अल्गोरिदम](https://en.wikipedia.org/wiki/Pattern-searching_algorithm) का उपयोग करती हैं जो पैटर्न पहचान और स्थान पर रिप्लेस करने में मदद करते हैं। रेगुलर एक्सप्रेशन के अधिक शुद्ध [विशलेषण](https://ryanstutorials.net/regular-expressions-tutorial/) क्योंकि उन्हें एक अल्गोरिदम या परिसर के रूप में डीबग किया जा सकता है।

## इसके अलावा देखें:
रेगुलर एक्सप्रेशन के बारे में अधिक जानने के लिए आप [गोलंग पैकेज डॉक्यूमेंटेशन](https://golang.org/pkg/regexp/) का उपयोग कर सकते हैं जहाँ आपको एक उदाहरण और फ़ंक्शन की सूची मिलेगी। आप [regular-expressions.info](https://www.regular-expressions.info/) जैसी वेबसाइट से भी रेगुलर एक्सप्रेशन के बारे में व्यापक जानकारी प्राप्त कर सकते हैं।