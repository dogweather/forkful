---
title:                "Elm: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन तर्क पढ़ना"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि प्रोग्रामिंग का यह साधन क्यों महत्वपूर्ण हो सकता है? पूर्णांक तथा मान आदि को स्टोर करने के लिए अलग-अलग परामीटर को डालना अपने आप में एक चुनौतीपूर्ण काम हो सकता है। अतः डेस्कटॉप या वेब अनुप्रयोगों में ये कमांड लाइन आगठी महत्वपूर्ण हैं।

## कैसे करें

आपको प्रोग्रामिंग भाषा Elm में कमांड लाइन आगठी को पढ़ने के लिए निम्नलिखित चरणों का पालन करना होगा। पहले हम उन परामीटरों की डालन करते हैं जो हमें चाहिए, फिर हम उन्हें समझते हैं और अंत में आवश्यकतानुसार प्रिंट करते हैं।

``` Elm
import Platform

main : Program String
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type Msg
    = Param String
    | PrintOutput

init : () -> ( Model, Cmd Msg )
init _ =
    let
        args =
            Platform.workerContext.cmdLineArgs
    in
        ( Model args, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Param value ->
            ( Model value, Cmd.none )

        PrintOutput ->
            ( model, Cmd.none )

```

इस उदाहरण में हमने तीन परामीटर डाले हैं और उन्हें संग्रहीत किया है। फिर हमने संग्रहीत परामीटर को print किया है जो प्रिंटर और प्रिंटर टिकानेर पहले से ही उपलब्ध हैं।

## गहराई में जाएं

अगर आप अपने प्रोग्राम में कमांड लाइन आगठी को ज्यादा मूल्य देना चाहते हैं, तो आप और गहराई में जा सकते हैं। आप अन्य भाषाओं में भी कमांड लाइन आगठी को पढ़ सकते हैं, लेकिन अगर आप Elm में कर रहे हैं, तो आपको कुछ भ्रांतियों क