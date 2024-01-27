---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:14:48.485613-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रीड-इवैल-प्रिंट लूप (REPL) एक सरल, संवादात्मक प्रोग्रामिंग वातावरण है जो एकल उपयोगकर्ता इनपुट्स को लेता है, उनका मूल्यांकन करता है, और उपयोगकर्ता को परिणाम लौटाता है। Elm प्रोग्रामर REPL का उपयोग त्वरित प्रयोगों, डिबगिंग, या भाषा सीखने के लिए करते हैं।

## कैसे करें:
Elm में एकीकृत REPL नहीं आता। हालांकि, Elm स्थापित करने के बाद आप अपनी कमांड लाइन से `elm repl` का उपयोग करके Elm सेशन शुरू कर सकते हैं।

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

इस सत्र में, List फ़ंक्शन्स को इंपोर्ट करने के बाद, हमने एक लिस्ट में नंबर्स को दोगुना कर दिया और तुरंत परिणाम प्राप्त किया।

## गहराई से जानकारी
Elm का REPL कुछ अन्य भाषाओं जैसे कि Python या JavaScript के REPL की तुलना में सीमित प्रतीत हो सकता है, क्योंकि Elm एक संकलित भाषा है जो वेब ऐप्स बनाने पर केंद्रित है। ऐतिहासिक रूप से, Elm ने पूर्ण अनुप्रयोगों पर ध्यान केंद्रित किया है बजाय स्क्रिप्टिंग या शेल इंटरैक्शन्स के।

Elm के REPL के विकल्पों में `elm-live` और Ellie जैसे ऑनलाइन संपादक शामिल हैं जहाँ आप ब्राउज़र में वास्तविक समय में कोड में परिवर्तन को देख सकते हैं।

लागू करने के संबंध में, Elm REPL पृष्ठभूमि में Elm कोड के टुकड़ों को JavaScript में संकलित करता है, जिससे आप Elm को संवादात्मक रूप से चला सकते हैं। यह व्याख्या की गई भाषाओं के REPL से अलग है, जिन्हें इस संकलन चरण की आवश्यकता नहीं होती है। Elm REPL को मूल भाषा को हल्का और केंद्रित रखने के लिए साधारण बनाया गया है।

## देखें भी
- Elm की आधिकारिक गाइड इंटरैक्टिविटी पर: https://guide.elm-lang.org/interop/
- Ellie, एक ऑनलाइन Elm प्लेग्राउंड: https://ellie-app.com/new
- `elm-live`, Elm के लिए एक लचीला डेव सर्वर: https://www.elm-live.com/