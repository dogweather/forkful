---
title:                "Elm: वर्तमान तारीख प्राप्त करना"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि अपने प्रोग्राम में वर्तमान तारीख कैसे प्राप्त किया जाए? यह बहुत आसान है और Elm में सुविधाजनक तरीके से किया जा सकता है। जानने के लिए आगे पढ़ें!

## कैसे करें

```Elm
import Time exposing (..)
import Date exposing (..)

getDate : Date
getDate =
    Time.now |> Time.toPosix |> Date.fromPosix
```

ऊपर दिए गए कोड स्निपेट में, हमने `import` कमांड का उपयोग करके `Time` और `Date` मॉड्यूल को इम्पोर्ट किया है। फिर हमने `Time.now` फंक्शन का उपयोग करके प्राप्त समय को पोजिक्स में कन्वर्ट किया और उसे `Date.fromPosix` से तारीख में बदल दिया है। अब हम `Date` मॉड्यूल के द्वारा प्राप्त की गई तारीख को `getDate` नामक फंक्शन से प्राप्त कर सकते हैं। 

आपको तारीख को स्ट्रिंग फॉर्मेट में चाहे तो इसे प्रिंट भी कर सकते हैं। नीचे दिए गए कोड स्निपेट में, हमने `Html.text` का उपयोग करके तारीख को प्रिंट किया है। 

```Elm
import Html exposing (text)

main =
    text getDate
```

आपको इस तरह का आउटपुट मिलेगा:

`Jan 23, 2021`

## गहराई में जाएं

अगर आप और गहराई में जाना चाहते हैं तारीख को प्राप्त करने के लिए, तो पहला कदम है `Time` और `Date` मॉड्यूल को समझना। `Time` मॉड्यूल में, हम `now` और `toPosix` फंक्शन का उपयोग कर सकते हैं तारीख को पोजिक्स में कन्वर्ट करने के लिए। जबकि `Date` मॉड्यूल में, हम `fromPosix` और `toTime` फंक्शन का उपयोग कर सकते हैं। इन फंक्शन्स के अध