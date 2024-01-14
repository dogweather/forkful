---
title:                "Javascript: सबस्ट्रिंग निकालना"
simple_title:         "सबस्ट्रिंग निकालना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप एक विकासक हैं और एक स्ट्रिंग से उपस्थित कुछ हिस्सों को निकालने के लिए इच्छुक हैं, तो आप उपस्थित स्ट्रिंग से स्थानिक स्ट्रिंग का उपयोग करके काम कर सकते हैं।

## कैसे
```Javascript
// एक स्ट्रिंग बनाएं
var str = "मेरा नाम जॉन डो "
// स्थानिक स्ट्रिंग बनाएं
var substr = str.substr(7, 3) // "जॉन"
console.log(substr) // नत्स
```

## गहरा अन्वेषण
स्थानिक स्ट्रिंग के उपयोग से आप उपस्थित स्ट्रिंग के किसी भी हिस्से को आसानी से निकाल सकते हैं। आपको स्थानिक स्ट्रिंग के दूरी और लंबाई को निर्दिष्ट करने की आवश्यकता होती है और आप उन्हें पैरामीटर के रूप में पास कर सकते हैं। आप स्थानिक स्ट्रिंग के साथ समर्थन करते हैं और उन्हें आसानी से उपयोग कर सकते हैं।

## देखो भी
- [जावास्क्रिप्ट स्ट्रिंग स्लाइस का उपयोग कैसे करें](https://developer.mozilla.org/hi/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [जावास्क्रिप्ट स्ट्रिंगेलँथ का उपयोग कैसे करें](https://developer.mozilla.org/hi/docs/Web/JavaScript/Reference/Global_Objects/String/length)