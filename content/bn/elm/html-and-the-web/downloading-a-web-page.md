---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:45.140436-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 \u09B8\u09B0\
  \u09BE\u09B8\u09B0\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\
  \u09AA\u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\
  \u09BE\u09A4\u09C7 \u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u09AC\u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\
  \u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09B0\u09BF\u09AF\u09BC\u09C7\u09B2-\u099F\u09BE\u0987\u09AE \u09A4\u09A5\u09CD\
  \u09AF\u2026"
lastmod: '2024-03-17T18:47:43.947122-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 \u09B8\u09B0\u09BE\
  \u09B8\u09B0\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\
  \u09A4\u09C7 \u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u09AC\
  \u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09B0\
  \u09BF\u09AF\u09BC\u09C7\u09B2-\u099F\u09BE\u0987\u09AE \u09A4\u09A5\u09CD\u09AF\
  \u2026"
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কি এবং কেন?

ওয়েব পেজ ডাউনলোড করা মানে ইন্টারনেট থেকে সরাসরি আপনার অ্যাপে ডেটা নেওয়া যাতে তা প্রদর্শন বা প্রক্রিয়া করা যায়। প্রোগ্রামাররা এটি করে রিয়েল-টাইম তথ্য পেতে অথবা ব্যবহারকারীদের কাছে ডাইনামিক সামগ্রী সরবরাহ করার জন্য।

## কিভাবে:

Elm এ HTTP অনুরোধের মতো সাইড ইফেক্টগুলিকে কমান্ড হিসেবে গঠন করতে হয়। আপনি `Http` মডিউল ব্যবহার করে ডেটা ফেচ করবেন এবং রেসপন্স হ্যান্ডেল করবেন।

```Elm

module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPage "https://api.example.com/data"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get { url = url, expect = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | content = "Error: Could not fetch page." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

```

সফল ফেচের ক্ষেত্রে, আপনার মডেলের `content` এ পৃষ্ঠার বিষয়বস্তু থাকবে। ত্রুটির ক্ষেত্রে, এটি একটি সাধারণ ত্রুটি বার্তা ধারণ করবে।

## গভীর ডাইভ

Elm সাইড ইফেক্টকে একটি ডাটা হিসেবে ব্যবহার করে, যার মানে হল HTTP অনুরোধগুলি Elm রানটাইম দ্বারা পরিচালিত হয়, আপনার কোডে সরাসরি নয়। ঐতিহাসিকভাবে, এটি JavaScript এর মতো ভাষাগুলি থেকে একটি প্রত্যাহার ছিল, যেখানে সাইড ইফেক্টগুলি আরও ফ্রি-উইলিং। অন্যান্য ভাষায় বিকল্পগুলি হতে পারে JavaScript এর `fetch` অথবা Python-এর `requests`। Elm এর এই পদ্ধতি আপনার অ্যাপকে পূর্বানুমানযোগ্য এবং রক্ষণাবেক্ষণ যোগ্য রাখে সাইড ইফেক্টগুলিকে টাইপে এনকোড করে এবং পরিবর্তনগুলি পরিচালনা করার জন্য একটি কেন্দ্রীয় `update` ফাংশন ব্যবহার করে।

`Http` মডিউলটি Elm এ সর্বদা ছিল না। প্রাথমিক সংস্করণে AJAX নিজেই তৈরী করা হয়েছিল, যা বিব্রতকর ছিল। এখন, `Http` বিভিন্ন কেস হ্যান্ডল করার জন্য বিভিন্ন ফাংশন প্রদান করে, যেমন JSON অথবা স্ট্রিংগুলি প্রত্যাশা করে, যা এটিকে ব্যবহারকারীর জন্য বন্ধুত্বপূর্ণ করে তোলে।

বাস্তবায়নের দিক থেকে, `fetchPage` কল করার সময়, Elm আপনার `update` ফাংশনে ফলাফলের সাথে একটি বার্তা পাঠায়। এটি হয় `Ok data` হবে যদি এটি সফল হয় অথবা `Err error` যদি এটি ব্যর্থ হয়। আপনি এই ফলাফলগুলি সহ প্যাটার্ন-ম্যাচ করেন এবং আপনার `Model` এবং ভিউকে উপযুক্তভাবে আপডেট করেন।

## দেখুন এও

- Elm's HTTP প্যাকেজ ডকুমেন্টেশন: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm গাইড ইফেক্টস অন: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- Elm এ JSON ডিকোডিং (যখন আপনি ফেচ করতে যাচ্ছেন ডেটা একটি সাধারণ স্ট্রিং নয়): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
