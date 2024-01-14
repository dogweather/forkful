---
title:    "Elm: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट पढ़ना।"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

कई बार एक प्रोग्रामर को अपने एलम प्रोग्राम को त्वरित और आसान बनाने की जरूरत होती है, जो स्क्रिप्ट की स्थापना या अन्य उपयोगों के अनुसार गुणवत्ता को सुनिश्चित करता है। इसके लिए आमतौर पर, यह प्रोग्रामर के प्रोग्राम में सक्रिय तरीकों से डेटा प्रक्रिया और वितरण की जांच करने के लिए आवश्यक होता है।

## कैसे करें

एलम में कमांड लाइन तर्कों को पढ़ने के लिए बहुत सरल है। सबसे पहले, हम `Platform` और `Platform.Cmd` मॉड्यूल को आवश्यकतानुसार आवृत्त करेंगे। फिर, हम `Html.programWithFlags` के साथ आसानी से अपने काम को भक्तिभाव देने हेतु वर्ग संज्ञा को स्थापित कर सकते हैं।

```Elm
import Platform
import Platform.Cmd

main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```

इसके बाद, हम `Browser.element` के साथ अपनी वेब पेज को संज्ञानात्मक और बंद कर सकते हैं।

```Elm
type alias Model =
    {}

init : (Model, Cmd msg)
init =
    ( {}, Cmd.none )

type Msg
    = ...

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        ...

view : Model -> Html Msg
view model =
    ...

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
```

प्रोग्राम को पिनकोड करने पर कोड  `Browser.element` के साथ अपने लंबाई और चौड़ाई प्रदर्शित करेगा, ` init` स्थानापन्न के साथ बंद एक स्वचालित तरीके से अपने प्रोग्राम को स्वतंत्र नमूना संदूल और `update` मॉड्यूल के लिए `view` का प्रदर्शन करता है।

## डीप डाइव

अगर आपको उस स्थान पर एक आग की आँटि या एक अन्य प्रकार की बातान्यायन्ता के साथ एक प्रोग्राम के