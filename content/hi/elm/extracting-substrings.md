---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:43.864009-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
उप-स्ट्रिंग निकालना मूल स्ट्रिंग से विशिष्ट भाग का चयन करना है। प्रोग्रामर डेटा को संशोधित, दिखाने, या प्रोसेस करने के लिए उप-स्ट्रिंग्स का इस्तेमाल करते हैं।

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
