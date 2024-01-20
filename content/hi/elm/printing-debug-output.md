---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डीबग आउटपुट मुद्रित करना यह सुनिश्चित करता है कि कोड संचालन के दौरान क्या हो रहा है, ताकि प्रोग्रामर को समझ में आ सके। यह प्रोग्रामरों को कोड में त्रुटियां ढूंढने और उन्हें सुधारने में मदद करता है।

## कैसे करें:

```Elm
import Debug exposing (toString)

main =
    let
        output = "Hello, World!"
    in
    Debug.log "Output: " output
```
उपरोक्त कोड, आउटपुट को कन्सोल में प्रिंट करेगा: `Output: Hello, World!`.

## गहराई से जानें:

1. ऐतिहासिक प्रासंगिकता: डीबग लाइब्रेरी ने एल्म की घोषणात्मक प्रकृति को समर्थन करने के लिए `println` फ़ंक्शन का प्रयोग किया। इसने डीबगगिंग के प्रक्रिया को सरल और त्रुटियों को खोजने में मदद की। 

2. विकल्प: अन्य प्रोग्रामिंग भाषाओं में यूज़ किए जाने वाले डीबग्गिंग टूल्स हैं `console.log()` (JavaScript), `print()` (Python), और `System.out.println()` (Java)। 

4. क्रियान्वयन विवरण: डीबग लॉग का प्रयोग "ट्रांसफॉर्म" करने वाला फ़ंक्शन है जो आपके डाटा को स्ट्रिंग में बदल देता है, ऐसा इसलिए क्योंकि Elm केवल स्ट्रिंग्स को कन्सोल में प्रिंट कर सकता है।

## इन्हें भी देखें:

1. [Elm गाइड: डीबग लॉग](https://guide.elm-lang.org/interop/ports.html)
2. [Stack Overflow: Elm में कन्सोल लॉग्गिंग कैसे करें](https://stackoverflow.com/questions/31428200/how-to-console-log-in-elm)
3. [Elm डॉक्यूमेंटेशन: Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)