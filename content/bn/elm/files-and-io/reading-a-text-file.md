---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:51.599303-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09AE\u09A8 \u098F\
  \u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u099F\u09BE\u09A8\u09BE \u09AF\
  \u09BE \u09AC\u09BE\u0987\u09A8\u09BE\u09B0\u09BF \u09A1\u09C7\u099F\u09BE\u09B0\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09C7 \u09AA\u09BE\u09A0\u09AF\u09CB\
  \u0997\u09CD\u09AF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09B9\u09BF\u09B8\u09C7\
  \u09AC\u09C7 \u0997\u09A0\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE, \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.966191-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09AE\u09A8 \u098F\u0995\
  \u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\u09B7\
  \u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u099F\u09BE\u09A8\u09BE \u09AF\u09BE\
  \ \u09AC\u09BE\u0987\u09A8\u09BE\u09B0\u09BF \u09A1\u09C7\u099F\u09BE\u09B0 \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09C7 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7\
  \ \u0997\u09A0\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE, \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কি এবং কেন?

টেক্সট ফাইল পড়া মানে হল এমন একটি ফাইল থেকে বিষয়বস্তু টানা যা বাইনারি ডেটার পরিবর্তে পাঠযোগ্য টেক্সট হিসেবে গঠিত। প্রোগ্রামাররা ডেটা, কনফিগারেশন, অথবা তাদের অ্যাপ্লিকেশনে বৃহৎ পরিমাণ টেক্সট আমদানি করার জন্য টেক্সট ফাইল পড়ে।

## কিভাবে:

Elm মূলত ফ্রন্ট-এন্ড ওয়েব ডেভেলপমেন্টে মনোনিবেশ করে থাকে, যেখানে সরাসরি ফাইল সিস্টেম অ্যাক্সেস করা নিরাপত্তার কারণে একটি না-যাওয়া পথ। এর পরিবর্তে, আপনি ব্যবহারকারীদের দ্বারা ফাইল আপলোড নিয়ন্ত্রণ করেন। এখানে আপনি কিভাবে একটি টেক্সট ফাইল পড়তে পারেন যা একজন ব্যবহারকারী নির্বাচন করে:

```Elm
module Main exposing (..)

import Browser
import File exposing (File)
import File.Selector as Selector
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model =
    { fileContent : String }

type Msg
    = SelectFile
    | ReceiveFileContent (Result () String)

init : Model
init =
    { fileContent = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFile ->
            (model, fileSelectCmd)

        ReceiveFileContent (Ok content) ->
            ({ model | fileContent = content }, Cmd.none)

        ReceiveFileContent (Err _) ->
            (model, Cmd.none)

fileSelectCmd : Cmd Msg
fileSelectCmd =
    File.select [ Selector.accept "text/*" ] { onDone = ReceiveFileContent }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Select a text file" ]
        , div [] [ text model.fileContent ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
```

আপনার ব্রাউজারে কোডটি চালান, বোতামে ক্লিক করুন, এবং একটি টেক্সট ফাইল নির্বাচন করুন। এটি আপনার Elm অ্যাপ্লিকেশনে বিষয়বস্তু প্রদর্শন করে।

## গভীর ডুব

Elm সরাসরি সার্ভারের ফাইল সিস্টেম থেকে ফাইল পড়ে না - এটি সার্ভার-সাইড অপারেশনের জন্য ডিজাইন করা হয়নি। এর পরিবর্তে, Elm ব্রাউজারের File API এর মাধ্যমে ফাইল ইনপুট পরিচালনা করে, সাধারণত ব্যবহারকারীর কোনো কর্মের দ্বারা ট্রিগার হয়, যেমন ফাইল নির্বাচন অথবা ড্র্যাগ-এন্ড-ড্রপ কর্ম। এটি একটি নিরাপত্তা পদক্ষেপ।

অতীতে, আপনি সার্ভার-সাইডে ফাইল পড়ার জন্য জাভাস্ক্রিপ্ট এবং Node.js ব্যবহার করে থাকতে পারেন, অথবা XMLHttpRequest (XHR) ব্যবহারকারীর ইন্টারঅ্যাকশন ছাড়া ক্লায়েন্ট-সাইড পড়ার জন্য। এগুলির নিরাপত্তা মডেল এবং ক্ষমতা ভিন্ন।

Elm এর `File` এবং `File.Selector` মডিউলগুলি ব্রাউজারে ফাইল পড়ার হ্যান্ডল করা যথেষ্ট মসৃণ করে তোলে, তবে Elm এর "কোনো পার্শ্ব প্রভাব নেই" দর্শন মনে রাখুন। এর মানে হল ফাইল পড়া কঠোরভাবে নিয়ন্ত্রিত, স্পষ্ট ব্যবহারকারীর ক্রিয়াকলাপের প্রয়োজন। এছাড়াও, ফাইলের বিষয়বস্তু পার্সিং এবং ডিকোডিং যত্ন সহকারে করা দরকার যাতে Elm এর শক্ত টাইপিং ম্যাচ করে।

## আরও দেখুন

- অফিসিয়াল Elm ফাইল API ডকুমেন্টেশন: https://package.elm-lang.org/packages/elm/file/latest/
- Elm এর কমান্ড এবং সাবস্ক্রিপশন গাইড (অ্যাসিনক্রোনাস অপারেশন বোঝার জন্য): https://guide.elm-lang.org/effects/
- Elm Discuss প্রশ্ন এবং কমিউনিটি ইন্টারঅ্যাকশনের জন্য: https://discourse.elm-lang.org/
