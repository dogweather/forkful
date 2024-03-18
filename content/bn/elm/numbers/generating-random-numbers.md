---
title:                "এলোমেলো সংখ্যা উৎপন্ন করা"
date:                  2024-03-17T17:51:01.982176-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি & কেন?

এলমে র‍্যান্ডম সংখ্যা তৈরি করা মানে `Random` মডিউল ব্যবহার করে প্রতিরূপ র‍্যান্ডম সংখ্যা উৎপাদন করা, যা বিভিন্ন ধরণের কাজে যেমন গেমস, সিমুলেশন এবং এমনকি স্টোকাস্টিক প্রক্রিয়ার দাবি রাখা অ্যালগরিদমের অংশ হিসেবেও কাজে লাগে। এই ক্ষমতা ডেভেলপারদের তাদের অ্যাপ্লিকেশনে অনিশ্চয়তা এবং বৈচিত্র্য যোগ করতে দেয়, যা ব্যবহারকারীর অভিজ্ঞতা এবং কার্যকারিতা বাড়াতে সাহায্য করে।

## কিভাবে:
এলমের পিউর ফাংশনাল প্রকৃতির কারণে আপনি সরাসরি র‍্যান্ডম সংখ্যা তৈরি করতে পারবেন না যেমনটা আপনি ইম্পেরেটিভ ভাষায় করতে পারেন। পরিবর্তে, আপনাকে `Random` মডিউলটি কমান্ডসের সহযোগে ব্যবহার করতে হবে। এখানে একটি বেসিক উদাহরণ দেওয়া হল যেটি 1 থেকে 100 এর মধ্যে একটি র‍্যান্ডম পূর্ণসংখ্যা তৈরি করে।

প্রথমে, `elm install elm/random` দিয়ে `Random` মডিউলটি ইনস্টল করুন। তারপর আপনার এলম ফাইলে এটি ইম্পোর্ট করুন, প্রয়োজনীয় HTML এবং ইভেন্ট মডিউলগুলির সাথে, এরকম:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

এটি একটি স্বয়ংসম্পূর্ণ উদাহরণ হতে, আপনি এই বয়লারপ্লেট যোগ করতে পারেন:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

তারপর, একটি **কমান্ড** সংজ্ঞায়িত করুন যেটি একটি র‍্যান্ডম সংখ্যা তৈরি করবে। এটি একটি `Msg` টাইপ সেটআপ করে যখন র‍্যান্ডম সংখ্যাটি উৎপন্ন হয়, একটি `Model` তা সংরক্ষণ করতে, এবং সবকিছুকে একসাথে বাঁধার জন্য একটি আপডেট ফাংশন সেটআপ করে।
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

একটি সংখ্যা উৎপন্ন করতে, আপনি একটি `Generate` মেসেজ পাঠাতে চাইবেন, উদাহরণস্বরূপ, আপনার ভিউতে একটি বোতামের মাধ্যমে:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

আপনি যখন "Generate" বোতামে ক্লিক করবেন, 1 থেকে 100 এর মধ্যে একটি র‍্যান্ডম সংখ্যা প্রদর্শিত হবে।

এই সহজ প্রক্রিয়াটি অন্যান্য ফাংশন ব্যবহার করে রূপান্তরিত ও বিস্তৃত করা যেতে পারে `Random` মডিউলে, র‍্যান্ডম ফ্লোটস, লিস্ট, বা এমনকি কাস্টম টাইপে ভিত্তি করে জটিল ডাটা স্ট্রাকচার উৎপাদন করতে, যা আপনার এলম অ্যাপ্লিকেশনে অনিশ্চয়তা যোগ করার জন্য বিস্তৃত খেলার মাঠ প্রদান করে।

এলম গাইডে আরও বিস্তারিত আছে। এতে [একটি ছয়-পাশের মরা রোলিংয়ের একটি উদাহরণও আছে](https://guide.elm-lang.org/effects/random).
