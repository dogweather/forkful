---
title:                "Elm: HTML का अनुश्लेषण"
simple_title:         "HTML का अनुश्लेषण"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

HTML को पार्स करने के क्या फायदे हो सकते हैं? यह एक सामान्य सवाल है जो हर एक प्रोग्रामर के मन में हो सकता है। प्रोग्रामिंग के कई क्षेत्रों में HTML को पार्स करने के लिए बहुत उपयोगी उपकरण हो सकते हैं। वैसे तो HTML को सीधे पार्स करना मुमकिन है, लेकिन Elm में पार्सिंग का तरीका सरल और आसान हो सकता है।

## कैसे करें

हम इस कोड ब्लॉक में शुरू करेंगे, जहां हम Elm का उपयोग करके HTML को पार्स करेंगे।

```Elm
import Html.Parser exposing (..)

sampleHtml = """
<html>
  <head>
    <title>My Elm Blog</title>
  </head>
  <body>
    <h1>Introduction to Parsing HTML in Elm</h1>
    <p>HTML को पार्स करने के लिए Elm उपयोग करना बहुत ही आसान है।</p>
  </body>
</html>
"""

parsedHtml = parse sampleHtml

main = 
    case parsedHtml of
        Ok parsed ->
            text <| "Title: " ++ (toString <| title parsed)
        Err error ->
            text "Error parsing HTML"

```

इस कोड से हमें इस प्रकार का आउटपुट मिलेगा:

```
Title: My Elm Blog
```

यहां `import Html.Parser exposing (..)` हमें `Html.Parser` मॉड्यूल से सभी फंक्शन को एक्सपोस करने की अनुमति देता है। पहले हम एक सामान्य HTML स्ट्रिंग को `sampleHtml` में सेट करते हैं। फिर हम `parse` फंक्शन का उपयोग करते हुए इस स्ट्रिंग को पार्स करते हैं और अंत में `main` फंक्शन में इसे प्रिंट करते हैं।

## गहराई में

HTML को पार्स करने का एक अध्ययन अधिकांश तरह के डेटा टाइप को पार्स करने के लिए संरचित तरीके में बदल सकते हैं। इससे आप अपने कोड को स्पष्ट और उचित बना सकते हैं। आप एलम में पार्सिंग के लिए `Html.Parser` मॉड्यूल के साथ एक सु