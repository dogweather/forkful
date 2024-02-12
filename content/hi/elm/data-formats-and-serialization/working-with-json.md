---
title:                "JSON के साथ काम करना"
aliases:
- /hi/elm/working-with-json.md
date:                  2024-02-03T19:23:38.743839-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Elm में JSON के साथ काम करने का अर्थ है JSON डेटा को Elm प्रकारों में डिकोड करना और Elm मानों को वापस JSON में एन्कोड करना। यह प्रक्रिया वेब एप्लिकेशनों को APIs और बाहरी डेटा सूत्रों के साथ बातचीत करने के लिए महत्वपूर्ण है, जो ग्राहक (Elm) और सर्वर या अन्य सेवाओं के बीच डेटा के सहज आदान-प्रदान की अनुमति देता है।

## कैसे करें:

Elm, JSON को संभालने का विशेष रूप से और सुरक्षा के साथ ध्यान रखता है, मुख्य रूप से `Json.Decode` और `Json.Encode` माड्यूल्स का उपयोग करता है। JSON के साथ काम करना शुरू करने के लिए, आपको सबसे पहले अपने डेटा प्रकार के लिए एक डिकोडर परिभाषित करना होगा। मान लीजिए कि हम एक साधारण उपयोगकर्ता प्रोफ़ाइल ऑब्जेक्ट के साथ काम कर रहे हैं।

सबसे पहले, अपने Elm प्रकार को परिभाषित करें:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSON को Elm में डिकोड करना

`UserProfile` प्रकार में एक JSON स्ट्रिंग को डिकोड करने के लिए, एक डिकोडर बनाएं:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

एक JSON ऑब्जेक्ट को डिकोड करने के लिए:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- नमूना उत्पादन:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Elm को JSON में एन्कोड करना

Elm मान को वापस JSON में एन्कोड करने के लिए, `Json.Encode` मॉड्यूल का उपयोग करें।

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{-
उपयोग:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

नमूना उत्पादन:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### थर्ड-पार्टी लाइब्रेरीज

`elm-json-decode-pipeline` जैसे Elm पैकेज पाइपलाइन शैली का उपयोग करके डिकोडर्स की सृजन को सरल बना सकते हैं, जो जटिल ऑब्जेक्ट्स को डिकोड करने के लिए विशेष रूप से सहायक है।

सबसे पहले, अपने प्रोजेक्ट में लाइब्रेरी जोड़ें:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

फिर, आप डिकोडर परिभाषा को इस प्रकार सरल बना सकते हैं:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- इस डिकोडर का उपयोग पहले की तरह decodeString के साथ JSON स्ट्रिंग्स को डिकोड करने के लिए करें। -}
```

यह दृष्टिकोण डिकोडर को सरल बनाता है, कोड को स्वच्छ और अधिक रखरखाव योग्य बनाता है, विशेष रूप से जैसे-जैसे डेटा संरचनाएं अधिक जटिल होती जाती हैं।
