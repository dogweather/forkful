---
title:                "हार्डवेयर को समझना"
html_title:           "Elm: हार्डवेयर को समझना"
simple_title:         "हार्डवेयर को समझना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## हम क्या है और क्यों करते हैं?
HTML को पार्सिंग करना क्या है, यह जानने के लिए हमें समझना होगा कि यह कैसे काम करता है। पार्सिंग का अर्थ है कि हम HTML डॉक्यूमेंट को पढ़ते हैं और उसमें दिए गए टैग और एट्रीब्यूट्स को समझते हैं। इसके माध्यम से हम वेब कंपोनेंट्स को बना सकते हैं जो कि हमारे द्वारा बनाए गए हैं।

## कैसे करें:
```Elm
import Html
import Html.Attributes

view : Html msg
view =
    Html.div [ Html.Attributes.id "container" ] [ Html.text "Hello World!" ]
```

यह एक बुनियादी Elm का कोड है जो कि एक डिव के भीतर है जिसमें हमने "container" नामक एट्रीब्यूट्स के साथ एक "Hello World!" टेक्स्ट को प्रिंट किया है। इस टेक्निक से हम बहुत ही आसानी से HTML को पार्स कर सकते हैं और अपने इंटरैक्टिव वेबसाइट का निर्माण कर सकते हैं।

## गहराई में:
(1) HTML के आते ही, बहुत से डिजाइनर्स ने कई सारे जैसे कि Adobe Dreamweaver आदि में WYSIWYG एडिटर का प्रयोग करके वेबसाइट बनाने की कोशिश की। इन एडिटर्स का उपयोग करने से काफी समय और पैसे बच सकते हैं। (2) पार्सिंग के लिए और एल्टर्नेटिव्स क्या सोचने हैं - क्या पार्सिंग को कोई भी समस्याएं हैं? (3) कैसे Elm के आते ही, डीविलोपर्स को पार्सिंग HTML का समाधान मिल गया है। क्या आप यह जानते हैं कि Elm क्यों पार्सिंग HTML के लिए इतना उत्तेजित है?

## अधिक देखें:
- [Elm ऑफिशियल वेबसाइट] (https://elm-lang.org/)
- [Elm की डॉक्यूमेंटेशन] (https://package.elm-lang.org/packages/elm/html/latest)
- [Elm की Github Repository] (https://github.com/elm/compiler)
- [Elm की पार्सिंग की विस्तृत जानकारी] (https://guide.elm-lang.org/)