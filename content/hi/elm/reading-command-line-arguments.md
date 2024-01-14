---
title:                "Elm: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट पढ़ना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट पढ़ना।"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप कोडिंग के अंदर नए हैं और प्रोग्रामिंग भाषा Elm को सीखना चाहते हैं, तो आप जानना चाहेंगे कि command line arguments क्या होते हैं और इन्हें कैसे पढ़ा जाता है। यह एक महत्वपूर्ण जानकारी है जो आपको प्रोग्रामिंग में आगे बढ़ने में मदद कर सकती है।

## कैसे करें
एक सरल उदाहरण के साथ, हम दिखाएंगे कि आप कैसे अपने Elm प्रोग्राम में command line arguments को पढ़ सकते हैं।

```Elm 
import Platform exposing (worker)
import Array exposing (toList, getAt)

main : Program Never Flags
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : () -> (Array String, Cmd Msg)
init _ =
    ( Platform.arguments, Cmd.none )

type Msg
    = ArgumentsMsg Array String

update : Msg -> Array String -> ( Array String, Cmd Msg )
update msg arguments =
    ( arguments, Cmd.none )

subscriptions : Array String -> Sub Msg
subscriptions arguments =
    Sub.none

```

उपरोक्त कोडब्लॉक में, हमने सर्वर से विन्यास किया हुआ worker फ़ंक्शन है, जो कि `Platform` में है और हमारे आगेबार आम argument सूची में सेट करता है। हमने `init` फ़ंक्शन को परिवर्तित किया है ताकि वह पोर्ट मानचित्र को खुश कर सकें, जिसमे हमने यहां से आगेबार आम प्राप्त कर लिए हैं कि आप Elm को अपने कोड के बाहर कैसे उपयोग कर सकते हैं।

मानचित्र का वर्गीकरण करने के बाद, हमने `update` फ़ंक्शन को लपेट किया हुआ है जिसमे हमने क्रमशः `arguments` संग्रह में समावेशित किया है।

## गहराई में जाएं
command line arguments को पढने के बारे में अधिक जानकारी के लिए, आप `Platform.arguments` और `Array` के लिए Elm निर्देशिका को देख सकते हैं। आप command line arguments को पढ़ने की विश्वसनीय प्रक्रिया और उसका उपयोग कर