---
title:    "Elm: स्ट्रिंग्स को मिलाकर बनाने की क्रिया"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## कॉनकैटनेटिंग स्ट्रिंग क्यों करें

कई बार आपको अपने कोड में दो स्ट्रिंग्स को एक साथ जोड़ने की आवश्यकता होती है। इसके लिए, आप बस एक स्ट्रिंग के अंत में दूसरी स्ट्रिंग को जोड़ सकते हैं।

## कैसे करें

इसके लिए, आपको आरंभिक आईपी के प्रौद्योगिकी को अपने अनुक्रम आदेश में जोड़ने की आवश्यकता होती है।

``` Elm
message = "Hello"
name = "John"

concat = message ++ ", " ++ name

```
**आउटपुट:** "Hello, John"

## गहराई से जाने

अगर आपने एलम में स्ट्रिंग एपेंड केलिएत करने की पूरी तरह से धंधन की है, तो आप जानते होंगे कि `++` अभिनेत्र के साथ प्रलम्बत् में भाग जुड़ जाता है। इसलिए, आप हमेशा ध्यान रखें कि स्ट्रिंग में स्लोट करूर रोनी चाहते हैं या गहराई से भूरो स्थानी को ज्यादा स्पर्श

अतिरिक्त सुझाव के लिए, आप इस वीडियो को देख सकते हैं:

[एलम में स्ट्रिंग एपेंडीका कुछ गहराई से जानें](https://www.youtube.com/watch?v=GYf1JWWbl_A)

## देखें भी

- [एलम अपनाऊ क्यों करें](https://www.programminghindi.in/why-use-elm/)
- [एलम बेसिक्स - एक संक्षेपण](https://medium.com/programming-hindi/elm-basics-a-summary-1f1f4ce7e69d)