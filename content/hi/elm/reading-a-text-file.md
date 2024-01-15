---
title:                "एक पाठ फ़ाइल को पढ़ना"
html_title:           "Elm: एक पाठ फ़ाइल को पढ़ना"
simple_title:         "एक पाठ फ़ाइल को पढ़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

गूगल के जाने माने इंजीनियर और वेब डेवलपर राजीवाकार, ने एक प्रश्न पूछा: क्या हम स्ट्रिंग्स या टेक्स्ट फ़ाइल्स को पढ़ने का तरीका सीख सकते हैं? यह चुनौतीपूर्ण हो सकता है लेकिन Elm में आप अपने कोड को स्ट्रिंग के साथ सम्मिलित करते हुए टेक्स्ट फ़ाइल्स को आसानी से पढ़ सकते हैं। तो अगर आप एक नए अभिगमन गतिविधि को शुरू करना चाहते हैं जो टेक्स्ट फ़ाइल्स को पढ़ता है, तो आप सही जगह पर हैं।

## कैसे करें

आइए हम अपने टेक्स्ट फ़ाइल को पढ़ने के लिए कुछ कोडिंग उदाहरण देखें। सबसे पहले, हम एक `elm/file` पैकेज को इंपोर्ट करेंगे। तो हम इसे `as File` नाम से एलियस कर सकते हैं ताकि हम उसे आसानी से उपयोग कर सकें। फिर हम जो फ़ाइल चाहें उसका पथ दर्ज करेंगे और `File.toUrl` फ़ंक्शन का उपयोग `url` नामक एक पुर्नश्चयी के रूप में करेंगे। अंत में, हम `send` को `init` फ़ंक्शन से बाध्य करते हैं जो हमारे `update` पैरामीटर में होगा जो हमारे शुरूआतिक मॉडल में होगा।

```Elm
import File as File

view : Model -> Html Msg
view model =
    div []
        [ (File.Encode.url model.filePath)
            `asUrl`
            ! [ onChange SelectFile ]
        , button [ onClick LoadFile ] [ text "Load File" ]
        ]

SelectFile : String -> Msg
SelectFile path =
    Update FilePath path

LoadFile : Msg
LoadFile =
    UpdateFile
```

जैसा कि आप देख सकते हैं, हमने अपने `view` फ़ंक्शन में एक `div` को प्रस्तुत किया है जिसमें हमारा एक `button` और एक `input` एलिमेंट ह