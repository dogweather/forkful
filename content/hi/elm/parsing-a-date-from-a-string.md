---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Date parsing सादा अर्थ में "एक string से दिनांक पाठ्य करना" होता है। यह इसलिए काम में लिया जाता है ताकि कार्यक्रम दिनांकों को सही तरीके से प्रदर्शित और रूपांतरित कर सके। 

## कैसे:

Elm में, आप `Date.fromString` का उपयोग करके इसे कर सकते हैं।

```Elm
import Date
import Json.Decode as Decode

main =
    let
        date =
            Result.withDefault Date.zero <|
                Date.fromString "2013-04-01T22:41:51.607Z"
    in
    Debug.log "date" date
```

यह डिवाइसर इस "2013-04-01T22:41:51.607Z" string को एक `Date` में परिवर्तित करेगा। 

## गहराई

Historical context में, Date parsing एक महत्वपूर्ण विषय रहा है इसलिए codes को मानकीकृत किया गया है। अन्य alternatives जैसे की `Date.Parse` आदि का उपऔग भी हो सकता है। `Date.fromString` प्रशोधन को बहुत अधिक सरल और सुनिश्चित करता है क्योंकी यह ISO8601 मानक का पालन करता है

## और देखें

Elm दस्तावेज़ीकरण (https://package.elm-lang.org/packages/elm/time/latest/Date).
ISO 8601 मानक (https://www.iso.org/iso-8601-date-and-time-format.html).