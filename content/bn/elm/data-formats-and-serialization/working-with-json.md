---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:25.946402-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm \u099C\u09C7\u09B8\u09A8 \u09B9\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982\u0995\u09C7 \u09B8\u09CD\u09AA\
  \u09B7\u09CD\u099F\u09A4\u09BE \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09BE\u09AA\
  \u09A4\u09CD\u09A4\u09BE \u09A6\u09BF\u09AF\u09BC\u09C7 \u099F\u09CD\u09B0\u09BF\
  \u099F \u0995\u09B0\u09C7, \u09AE\u09C2\u09B2\u09A4 `Json.Decode` \u098F\u09AC\u0982\
  \ `Json.Encode` \u09AE\u09A1\u09BF\u0989\u09B2\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 JSON \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09B6\u09C1\u09B0\u09C1\u2026"
lastmod: '2024-03-17T18:47:43.971497-06:00'
model: gpt-4-0125-preview
summary: "Elm \u099C\u09C7\u09B8\u09A8 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\
  \u09BF\u0982\u0995\u09C7 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\u09A4\u09BE \u098F\
  \u09AC\u0982 \u09A8\u09BF\u09B0\u09BE\u09AA\u09A4\u09CD\u09A4\u09BE \u09A6\u09BF\
  \u09AF\u09BC\u09C7 \u099F\u09CD\u09B0\u09BF\u099F \u0995\u09B0\u09C7, \u09AE\u09C2\
  \u09B2\u09A4 `Json.Decode` \u098F\u09AC\u0982 `Json.Encode` \u09AE\u09A1\u09BF\u0989\
  \u09B2\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7\u0964 JSON \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09A1\u09C7\u099F\u09BE \u099F\u09BE\u0987\u09AA\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u0995\u099F\u09BF \u09A1\u09BF\u0995\u09CB\u09A1\u09BE\u09B0 \u09A1\
  \u09BF\u09AB\u09BE\u0987\u09A8 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7\u0964\
  \ \u09A7\u09B0\u09BE \u09AF\u09BE\u0995, \u0986\u09AE\u09B0\u09BE \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0987\u0989\u099C\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u09CB\u09AB\u09BE\u0987\u09B2 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u099B\u09BF\u0964\
  \n\n\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 Elm \u099F\
  \u09BE\u0987\u09AA \u09A1\u09BF\u09AB\u09BE\u0987\u09A8 \u0995\u09B0\u09C1\u09A8\
  ."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
Elm জেসন হ্যান্ডলিংকে স্পষ্টতা এবং নিরাপত্তা দিয়ে ট্রিট করে, মূলত `Json.Decode` এবং `Json.Encode` মডিউলগুলি ব্যবহার করে। JSON নিয়ে কাজ করা শুরু করতে, আপনার প্রথমে আপনার ডেটা টাইপের জন্য একটি ডিকোডার ডিফাইন করতে হবে। ধরা যাক, আমরা একটি সাধারণ ইউজার প্রোফাইল অবজেক্ট নিয়ে কাজ করছি।

প্রথমে, আপনার Elm টাইপ ডিফাইন করুন:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSON কে Elm এ ডিকোড করা
`UserProfile` টাইপে একটি JSON স্ট্রিং ডিকোড করতে, একটি ডিকোডার তৈরি করুন:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

একটি JSON অবজেক্ট ডিকোড করতে:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- নমুনা আউটপুট:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Elm কে JSON এ এনকোড করা
Elm মানকে আবার JSON এ এনকোড করতে, `Json.Encode` মডিউল ব্যবহার করুন।

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
ব্যবহার:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

নমুনা আউটপুট:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### তৃতীয়-পক্ষের লাইব্রেরি
`elm-json-decode-pipeline` এর মতো Elm প্যাকেজগুলি পাইপলাইন স্টাইল ব্যবহার করে ডিকোডার তৈরি করতে সহজ করে, যা জটিল অবজেক্ট ডিকোড করার সময় বিশেষ করে সুবিধাজনক।

প্রথমে, আপনার প্রজেক্টে লাইব্রেরিটি যোগ করুন:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

তারপর, আপনি ডিকোডার ডেফিনেশনটি নিম্নরূপ সহজ করতে পারবেন:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- আগের মতো decodeString ব্যবহার করে JSON স্ট্রিংগুলি ডিকোড করার জন্য এই ডিকোডার ব্যবহার করুন। -}
```

এই পদ্ধতিটি ডিকোডারটিকে সহজ করে, কোডটিকে আরও পরিষ্কার এবং বজায় রাখার জন্য সুবিধাজনক করে, বিশেষ করে যখন ডেটা কাঠামোগুলি জটিল হয়ে ওঠে।
