---
title:                "Elm: कम्प्यूटर प्रोग्रामिंग में नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

क्यों: रेगुलर एक्सप्रेशन का उपयोग क्यों किया जाता है - रेगुलर एक्सप्रेशन एक सरल और शक्तिशाली तरीका है जो पाठ से पैटर्न मिलाने और संशोधित करने को संभव बनाता है। 

कैसे: "```Elm 
regex = Regex.fromString "[a-z]+"
Regex.find regex "hello123 world456" 
```" 
आउटपुट: "Just "hello""

डीप डाइव: रेगुलर एक्सप्रेशन का उपयोग करने के लिए और आगे बढ़ने के लिए जानने योग्य गहराई में जाएं। आप ब्राउज़र और सर्वर दोनों में रेगुलर एक्सप्रेशन का उपयोग कर सकते हैं और इसके कुछ उपयोगी चरण अनुसरण कर सकते हैं। 

देखें भी: जानिए और अधिक जानने के लिए अन्य विस्तृत लेख। 
- [Elm वेबसाइट] (https://elm-lang.org/)
- [क्वार्क] (https://package.elm-lang.org/packages/elm-community/regex/latest/)
- [RegExr] (https://regexr.com/)