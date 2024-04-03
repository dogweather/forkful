---
date: 2024-01-20 17:45:43.864009-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\
  \u0947\u0902 \u0909\u092A-\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `String.slice`\
  \ \u092A\u0926\u094D\u0927\u0924\u093F \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u0930\u0947\u0902."
lastmod: '2024-03-13T22:44:52.167436-06:00'
model: gpt-4-1106-preview
summary: "Elm \u092E\u0947\u0902 \u0909\u092A-\u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `String.slice` \u092A\u0926\u094D\u0927\u0924\u093F \u0915\u093E \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0947\u0902."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## How to: (कैसे करें:)
Elm में उप-स्ट्रिंग निकालने के लिए `String.slice` पद्धति का इस्तेमाल करें:

```Elm
import String

-- `String.slice` का इस्तेमाल कर उप-स्ट्रिंग निकालें
substringExample : String
substringExample =
    let
        originalString = "Hello, Elm programming!"
        startIndex = 7
        endIndex = 10
    in
    String.slice startIndex endIndex originalString

-- सैंपल आउटपुट
-- "Elm"
```

## Deep Dive (गहराई में जानकारी)
Elm में `String.slice` फंक्शन वर्शन 0.19 से उपलब्ध है और यह Python और JavaScript की स्लाइस पद्धतियों से प्रेरित है। वैकल्पिक तरीके में `String.left`, `String.right`, और `String.drop` शामिल हैं। स्ट्रिंग के एक विशिष्ट भाग को निकालते समय, `String.slice` मेमोरी में कॉपी बनाता है जिससे प्रोसेसिंग का समय बढ़ सकता है. इसके बावजूद, यह छोटी स्ट्रिंग्स के लिए उपयुक्त है।

## See Also (और देखें)
- Elm `String` पैकेज: [String package documentation](http://package.elm-lang.org/packages/elm/core/latest/String)
- Elm गाइड बुक: [An Introduction to Elm](https://guide.elm-lang.org/)
- Elm का `String.slice` शोध: [Exploring String.slice](https://elm-lang.org/docs/syntax#strings)
