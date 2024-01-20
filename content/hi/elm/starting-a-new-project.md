---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
नया प्रोजेक्ट शुरू करना सॉफ्टवेयर विकास की एक प्रक्रिया है जिसमें हम एक नई सॉफ्टवेयर उत्पाद की रचना करते हैं। प्रोग्रामर इसे ताजगी, नई सीख, और समस्या का समाधान प्राप्त करने के लिए करते हैं।

## कैसे करें:
एल्म प्रोजेक्ट शुरू करने के लिए, हमें एल्म पैकेज इंस्टॉल करने की आवश्यकता होती है।

```Elm
npm install -g elm
```

इसके बाद हम एक नया प्रोजेक्ट बना सकते हैं:

```Elm
elm init
```

नए "src" फोल्डर में, हमारा पहला कॉड होगा:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hello, Elm!"
```

यह प्रोग्राम "Hello, Elm!" प्रिंट करेगा।

## गहरा डाइव
एल्म, फ्रंट-एंड वेब डेवलपमेंट में मदद करने के लिए 2012 में लॉन्च किया गया था। इसके विकल्प में React, Angular आदि हैं, लेकिन एल्म में एक अद्वितीय वास्तुकला API और त्रुटि की बेहतर देखभाल है। एल्म प्रोजेक्ट की स्थापना `elm init` आदेश से की जाती है, जो बुनियादी डायरेक्टरी संरचना और Elm.json फ़ाइल बनाता है।

## अन्य देखें
1. [Elm Official Guide](https://guide.elm-lang.org/)
3. [Elm Vs Other](https://elm-lang.org/news/compilers-as-assistants)