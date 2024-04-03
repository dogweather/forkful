---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:38.743839-07:00
description: "Elm \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948\
  \ JSON \u0921\u0947\u091F\u093E \u0915\u094B Elm \u092A\u094D\u0930\u0915\u093E\u0930\
  \u094B\u0902 \u092E\u0947\u0902 \u0921\u093F\u0915\u094B\u0921 \u0915\u0930\u0928\
  \u093E \u0914\u0930 Elm \u092E\u093E\u0928\u094B\u0902 \u0915\u094B \u0935\u093E\
  \u092A\u0938 JSON \u092E\u0947\u0902 \u090F\u0928\u094D\u0915\u094B\u0921 \u0915\
  \u0930\u0928\u093E\u0964 \u092F\u0939 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\
  \u092F\u093E \u0935\u0947\u092C \u090F\u092A\u094D\u0932\u093F\u0915\u0947\u0936\
  \u0928\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:52.223351-06:00'
model: gpt-4-0125-preview
summary: "Elm \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948\
  \ JSON \u0921\u0947\u091F\u093E \u0915\u094B Elm \u092A\u094D\u0930\u0915\u093E\u0930\
  \u094B\u0902 \u092E\u0947\u0902 \u0921\u093F\u0915\u094B\u0921 \u0915\u0930\u0928\
  \u093E \u0914\u0930 Elm \u092E\u093E\u0928\u094B\u0902 \u0915\u094B \u0935\u093E\
  \u092A\u0938 JSON \u092E\u0947\u0902 \u090F\u0928\u094D\u0915\u094B\u0921 \u0915\
  \u0930\u0928\u093E\u0964 \u092F\u0939 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\
  \u092F\u093E \u0935\u0947\u092C \u090F\u092A\u094D\u0932\u093F\u0915\u0947\u0936\
  \u0928\u094B\u0902 \u0915\u094B APIs \u0914\u0930 \u092C\u093E\u0939\u0930\u0940\
  \ \u0921\u0947\u091F\u093E \u0938\u0942\u0924\u094D\u0930\u094B\u0902 \u0915\u0947\
  \ \u0938\u093E\u0925 \u092C\u093E\u0924\u091A\u0940\u0924 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\
  \u094D\u0923 \u0939\u0948, \u091C\u094B \u0917\u094D\u0930\u093E\u0939\u0915 (Elm)\
  \ \u0914\u0930 \u0938\u0930\u094D\u0935\u0930 \u092F\u093E \u0905\u0928\u094D\u092F\
  \ \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u092C\u0940\u091A \u0921\u0947\
  \u091F\u093E \u0915\u0947 \u0938\u0939\u091C \u0906\u0926\u093E\u0928-\u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\
  \u0947\u0924\u093E \u0939\u0948\u0964."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

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
