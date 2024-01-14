---
title:                "Elm: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV के साथ काम करने में आपको क्यों रूचि होनी चाहिए।

CSV (Comma Separated Values) फॉर्मेट सबसे आसान तरीका है डेटा को संग्रहीत करने का। इस फॉर्मेट में आप अपने डेटा को एक सरल स्प्रेडशीट की तरह प्रदर्शित कर सकते हैं जो कि प्रोग्रामर्स को अधिक सुविधा देता है। CSV फॉर्मेट को बहुत से प्रोग्रामिंग भाषाओं का समर्थन करता है लेकिन इसका समर्थन Elm में बहुत अच्छा है जो और भी रूचिकर है।

## कैसे

अब हम देखेंगे कि Elm में CSV को कैसे प्रबंधित किया जाए। यहां हम एक आसान से उदाहरण ले कर शुरू करेंगे:

``` Elm
import Csv
import File
import Html exposing (Html, text)

type alias User =
    { name : String
    , age : Int
    , occupation : String
    }

csvFile : String
csvFile =
    """
    name,age,occupation
    John,35,Engineer
    Jane,28,Teacher
    Bob,42,Writer
    """

parseUser : List String -> User
parseUser columns =
    { name = List.get 0 columns
    , age = String.toInt (List.get 1 columns) |> Result.withDefault 0
    , occupation = List.get 2 columns
    }

viewUser : User -> Html msg
viewUser user =
    Html.div []
        [ Html.text user.name
        , Html.text (String.fromInt user.age)
        , Html.text user.occupation
        ]

main : Html msg
main =
    csvFile
        |> Csv.Decode.decodeString Csv.HeaderRow
        |> Result.map (List.map parseUser >> viewUser)
        |> Result.withDefault (text "There was an error decoding the CSV file.")
```

उपरोक्त कोड एक CSV फ़ाइल से डेटा को पढ़ता है और उसे अलग-अलग `User` रूप में प्रदर्शित करता है। `parseUser` फ़ंक्शन फ़ाइल के प्रत्येक पंक्ति से स्थानीय `User` ऑब्जेक्ट बनाता है। इससे हमें अपने डेटा को उपयोग करने के लिए क्या आवश्यक है उसे समझने में सुविधा होती है।

अगर आप इस कोड को अपने सिस्टम में परीक्षण करते हैं तो आप इस प्रकार का आउटपुट देखेंगे: