---
title:                "नियमित अभिव्यक्तियों का प्रयोग करना"
html_title:           "Javascript: नियमित अभिव्यक्तियों का प्रयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का प्रयोग करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Regular Expressions का उपयोग करना एक तरह से code का उपयोग करना है, जो डाटा पर नियंत्रण रखने और उसे खोजने का एक तरीका है। प्रोग्रामर्स को इसका उपयोग इसलिए करना होता है क्योंकि ये उन्हें अपने कोड को अधिक साफ, संगठित और मजबूत बनाता है।

## कैसे करें:
आप जावास्क्रिप्ट में regular expressions का उपयोग करना चाहते हैं तो आप "RegExp" फंक्शन का उपयोग कर सकते हैं। इसका एक सरल उदाहरण है:
```Javascript
var pattern = /hello/;
console.log(pattern.test("hello world"));
```
इस कोड ब्लॉक में, हमने एक pattern बनाया है और उसे "hello world" string पर test किया है। यह true को रिटर्न करेगा क्योंकि string में "hello" शब्द मौजूद है।

## गहराई की खोज:
Regular Expressions को 1950 के दशक में जन्म दिया गया था। ये Ken Thompson और Dennis Ritchie द्वारा बनाया गया था, जो उस समय बेल लैबोरेट्रीज में काम कर रहे थे। इसके अलावा, regular expressions के दूसरे विकल्प भी मौजूद हैं, जैसे "match" और "search" methods। आप regular expressions को अपने आवश्यकताओं के अनुसार adjust कर सकते हैं, जो आसानी से प्रोग्रामिंग को अनुकूलित करता है।

## देखें भी:
- [MDN - Regular Expressions](https://developer.mozilla.org/hi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr - Online Regular Expression Testing Tool](https://regexr.com/)
- [Regular Expressions Cheat Sheet](https://www.shortcutfoo.com/app/dojos/regexp/cheatsheet)