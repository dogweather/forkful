---
date: 2024-01-20 17:38:54.909237-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\
  \u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u0915\u0928\u094D\u0935\
  \u0930\u094D\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `String.toLower`\
  \ \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0947\u0902\u0964."
lastmod: '2024-04-05T21:53:54.171011-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\u0947\u0902\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\u094B\u0905\
  \u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u0915\u0928\u094D\u0935\u0930\u094D\
  \u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `String.toLower`\
  \ \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0947\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

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
