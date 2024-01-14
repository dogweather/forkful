---
title:                "Elm: स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# प्रयोग क्यों करें
एल्म में स्ट्रिंग की लंबाई निकालने का एक प्रमुख कारण है कि अधिकांश प्रोग्राम इस जानकारी को अपनी कामनाों के अनुसार प्रयोग करते हैं। इससे प्रोग्रामिंग में आसान नहीं होता हैं, और हम वितरण के संदर्भ में चेतावनिया भेज सकते हैं।

## कैसे करें 
इस प्रयोग में, हम एल्म का "length" नमूना का प्रयोग करेंगे। इसमें हम स्ट्रिंग का लंबाई निकाल सकते हैं। नीचे दिए गए कोड ब्लॉक में प्रदर्शित होता है: 
```elm
import String
 
stringLength : String -> Int
stringLength s =
    String.length s

main =
    let
        result = stringLength "Hello"
    in
    Html.text (toString result) 
```
Output: 5

## गहराई में
स्ट्रिंग की लंबाई निकालना केवल कोड या कामना को पता करने से अधिक है। इसलिए, यदि आप इस प्रकार के कामना का उपयोग करने से पहले उसके विषय में अधिक जानना चाहते हैं, तो आप एल्म के अन्य प्रकार के दायरे ज़रूर खोजें। यह आपको इस विषय की गहराई और उससे जुड़े सभी मुद्दों को समझने में मदद करेगा। 

# इससे जुड़े आर्टिकल्स

[Elm राज्य का उपयोग करना: क्यों और कैसे](https://medium.com/@awanshrestha16/using-state-in-elm-why-and-how-f80d2b96b8db) 
[Elm में रेखांकन (Concatenation) कैसे करें](https://runelm.io/LEH5)