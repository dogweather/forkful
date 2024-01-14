---
title:                "Haskell: एचटीएमएल पार्सिंग"
simple_title:         "एचटीएमएल पार्सिंग"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

पार्सिंग HTML क्यों जरूरी है? यह दुनिया में यदि आप वेब से डेटा और जानकारी भांति तो आप शायद पार्सिंग HTML को जानें।

## कैसे करें

आपको सबसे पहले एक Haskell प्रोजेक्ट बनाना होगा। इसके बाद, आपको नीचे दिए गए कोड ब्लॉक में दिए गए कॉड को अपने प्रोजेक्ट में जोड़ना होगा।

```Haskell
import Text.HTML.TagSoup

main :: IO ()
main = do
    html <- readFile "sample.html"
    let tags = parseTags html
    let links = filter (\(TagOpen _ attrs) -> isLink attrs) tags
    let linksText = map (\(TagOpen _ attrs) -> fromAttrib "href" attrs) links
    putStrLn $ unlines linksText

isLink :: [(String, String)] -> Bool
isLink attrs = case lookup "href" attrs of
    Just _ -> True
    Nothing -> False
```
उपरोक्त कोड सारसंग्रह (tagSoup) कि आपके प्रोजेक्ट जोड़ना जरूरी होगा। यह आपको HTML कोड को हैंडल करने के लिए इस्तेमाल किया जाता है। इसके बाद, आपको "sample.html" फ़ाइल बनाना होगा जिसमें आप एक वेबपेज का HTML कोड पाएंगे। उपरोक्त कोड आपको उस HTML कोड से सभी लिंक्स का सारणी स्क्रिप्ट करेगा। आप यह संग्रह को अपनी आवश्यकता अनुसार बदल सकते हैं।

यह आपके कोड की एक छोटी स्नैपशॉट है। यह कैसे आप हस्केल में HTML पार्सिंग कर सकते हैं उसका एक विषय है।

## गहराई में

HTML पार्सिंग बहुत ही दिलचस्प है यहां तक कि मोबाइल अनुप्रयोगों और वेब साइटों पर HTML कोड को प्रसारित करने के बहुत सारे उपकरण हैं। इसके साथ ही, आप प्यार सिर्फ़ जीएसपी के साथ कोडिंग कर सकते हैं इसलिए यह आपको किसी भी अन्य भ