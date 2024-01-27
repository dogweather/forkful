---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

"## क्या और क्यों?"

रेगुलर एक्सप्रेशंस, या regex, पैटर्न मैचिंग के लिए इस्तेमाल होते हैं। प्रोग्रामर्स इन्हें टेक्स्ट सर्च करने, वैलिडेशन, और डेटा पर्सिंग के लिए इस्तेमाल करते हैं।

## How to:

"## कैसे करें:"

Elm में regex का उपयोग ऐसे करते हैं:

```Elm
import Regex exposing (Regex, fromString, contains)

checkEmail : String -> Bool
checkEmail email =
    case fromString "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" of
        Nothing ->
            False

        Just regex ->
            contains regex email

-- सैंपल आउटपुट:
checkEmail "valid@example.com" -- true
checkEmail "invalid@.com" -- false

```

## Deep Dive

"## गहराई में:"

रेगुलर एक्सप्रेशंस की उत्पत्ति 1950s में हुई थी। कंप्यूटर साइंस में ये पावरफुल उपकरण हैं। Elm में, Regex module का उपयोग पैटर्न मैचिंग के लिए होता है। विकल्प के रूप में string functions का इस्तेमाल कर सकते हैं, पर regex अधिक फ्लेक्सिबल होते हैं। Elm में regex को efficiently इंप्लिमेंट किया गया है ताकि पैटर्न मैचिंग तेज़ और आसान रहे।

## See Also

"## सम्बंधित जानकारी:"

- Elm के [official Regex module documentation](http://package.elm-lang.org/packages/elm/regex/latest) पर जाकर और पढ़ें।
- [Regex 101](https://regex101.com/) - रेगुलर एक्सप्रेशंस को ऑनलाइन टेस्ट और डीबग करने के लिए।
- [MDN Web Docs on Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - ब्राउज़र जावास्क्रिप्ट के संदर्भ में regex की जानकारी।
