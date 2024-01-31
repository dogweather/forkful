---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:54.909237-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक स्ट्रिंग को लोअर केस में कन्वर्ट करने का मतलब है, उस स्ट्रिंग के सभी अक्षरों को छोटे अक्षरों में बदल देना। इसकी जरूरत मुख्यत: डाटा की समरूपता और संवेदनशीलता को कम करने के लिए होती है, जैसे यूज़र इनपुट को स्टैंडर्डाइज़ करना या सर्च को संवेदनशीलता से मुक्त करना।

## How to: (कैसे करें:)
Elm में स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए `String.toLower` फंक्शन का उपयोग करें।

```Elm
import String

-- एक स्ट्रिंग को लोअर केस में बदलना 
lowerCaseString : String -> String
lowerCaseString str =
    String.toLower str

-- उपयोग का उदाहरण
main =
    let
        originalString = "Elm Programming LANGUAGE"
        lowerCased = lowerCaseString originalString
    in
    -- यह "elm programming language" प्रिंट करेगा
    text lowerCased
```

## Deep Dive (गहराई से जानकारी)
शुरुआती दिनों में, प्रोग्रामर्स को अपनी फंक्शंस खुद बनानी पड़ती थी। आज, Elm जैसी मॉडर्न भाषाओं में स्ट्रिंग ऑपरेशंस बहुत ही सरल हैं। `String.toLower` फंक्शन यूनिकोड स्टैंडर्ड का पालन करते हुए सभी अक्षरों को छोटे में बदल देता है जिससे विभिन्न भाषाओँ और स्क्रिप्ट्स में सामंजस्य बना रहता है। इसे प्रयोग करने के लिए किसी एक्सटर्नल लाइब्रेरी की जरूरत नहीं होती। अगर `String.toLower` आपकी जरूरतों को पूरा नहीं करता, तो Elm पैकेज्स आपकी मदद कर सकती हैं।

## See Also (सम्बंधित जानकारी)
- Elm `String` module documentation: [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode Standard for Case Mapping: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- Elm Package Catalog: [Elm Packages](https://package.elm-lang.org/) 

और जानकारी और विस्तार के लिए उपरोक्त लिंक्स का प्रयोग करें।
