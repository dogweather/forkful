---
title:                "HTMl को खोजना"
html_title:           "Elm: HTMl को खोजना"
simple_title:         "HTMl को खोजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि वेब पेज से डेटा निकालने के लिए कैसे प्रोग्रामिंग की जा सकती है? यदि हाँ, तो Elm आपको इस समस्या का समाधान प्रदान कर सकता है। यहाँ हम आपको बताएँगे कि क्यों Elm ही आपका सर्वोत्तम विकल्प हो सकता है।

## कैसे करें

### एलम में HTML पार्सिंग

एलम में HTML पार्सिंग साधन एक्सल वर्गों का उपयोग करके किया जा सकता है। नीचे दिए गए उदाहरण आपको एलम में HTML पार्सिंग करने की प्रक्रिया का अनुभव कराएंगे।

```elm
import Html exposing (text, Attribute, Node)
import Html.Parser exposing (parse, attribute, node)

-- एलम कोड से HTML अनुवाद करना
htmlString =
    "<div id='article'> <h1>Hello, World!</h1> <p>This is a sample paragraph.</p> </div>"

parsedHtml = parse htmlString

-- HTML के Node को उत्पन्न करने के लिए उपयोग अतिरिक्त चरित्र
Header1 =
    attribute "tagName" "h1" .. attribute "children" "Hello, World!" |> node

Paragraph =
    attribute "tagName" "p" .. attribute "children" "This is a sample paragraph." |> node

-- अंत में, HTML को Node तक रूपांतरण करके हम एक निष्पादन प्रक्रिया प्राप्त कर सकते हैं
-- parsedHtml वापस Json List देता है, जिसे हम एक मूल Node के रूप में परिवर्तित करते हैं
main =
    let
        extractedNodesInJsonListFormat =
            case parsedHtml of
                Ok successfulExtraction ->
                    successfulExtraction

                Err failedExtraction ->
                    [Header1, Paragraph]
    in
    text <| String.fromList extractedNodesInJsonListFormat
```

आउटपुट:

```elm
<div id='article'> <h1>Hello, World!</h1> <p>This is a sample paragraph.</p> </div>
```

संपूर्ण HTML को फ़ॉर्मेट किया गया है, जिसमें ```<`, `>` और `"` जैसे अतिरिक्त चरित्र शामिल हैं।

## भीड़ की गहराई में

एलम में HTML पर्सिंग को गहराई से देखने के लिए, आप एक "फ़ंक्शन" का उपयोग कर सकते हैं जो संदर्भ प्रायः