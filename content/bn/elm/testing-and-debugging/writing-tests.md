---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:18.766649-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Elm `elm-explorations/test` \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u0987\u0989\u09A8\u09BF\u099F \u098F\u09AC\u0982 \u09AB\u09BE\
  \u099C \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09A4, \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F\u09C7 \u098F\u0987 \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF \u09AF\u09CB\u0997 \u0995\u09B0\
  \u09A4\u09C7."
lastmod: '2024-03-17T18:47:43.952174-06:00'
model: gpt-4-0125-preview
summary: "Elm `elm-explorations/test` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0987\u0989\u09A8\u09BF\
  \u099F \u098F\u09AC\u0982 \u09AB\u09BE\u099C \u099F\u09C7\u09B8\u09CD\u099F \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09AA\u09CD\u09B0\u09A5\
  \u09AE\u09A4, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\
  \u09CD\u099F\u09C7 \u098F\u0987 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\
  \u09BF \u09AF\u09CB\u0997 \u0995\u09B0\u09A4\u09C7."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কীভাবে:
Elm `elm-explorations/test` প্যাকেজ ব্যবহার করে ইউনিট এবং ফাজ টেস্ট লেখার জন্য। প্রথমত, আপনার প্রজেক্টে এই প্যাকেজটি যোগ করতে:

```elm
elm install elm-explorations/test
```

একটি টেস্ট ফাইল তৈরি করুন, ধরুন `tests/ExampleTest.elm`, এবং টেস্টিং মডিউলগুলি ইম্পোর্ট করুন। এখানে একটি সহজ টেস্ট দেওয়া হলো যা `add : Int -> Int -> Int` ফাংশনটি যাচাই করে:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

আপনার টেস্ট চালাতে, আপনার `elm-test` প্রয়োজন হবে:

```shell
npm install -g elm-test
elm-test
```

এটি আপনার টেস্টগুলি কম্পাইল করবে এবং আপনার টার্মিনালে ফলাফল প্রিন্ট করবে। উপরে দেওয়া উদাহরণের জন্য, আউটপুট কিছু এইরকম হবে:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

একটি জটিল উদাহরণের জন্য, ধরুন আপনি `add` ফাংশনটিকে ফাজ টেস্ট করতে চান যাতে এটি সঠিকভাবে বিভিন্ন রেঞ্জের ইন্টিজার ইনপুট দিয়ে কাজ করতে পারে। আপনার `ExampleTest.elm` ফাইলটি আপনি যেমন পরিবর্তন করবেন:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

`elm-test` আবার চালান যদি আপনি ফাজ টেস্টগুলি ক্রিয়াকলাপে দেখতে চান। আউটপুট র্যান্ডম ইনপুটের সাথে পরিবর্তন হবে কিন্তু সফল টেস্টগুলি কোনো ব্যর্থতা না ইঙ্গিত করবে:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

এই উদাহরণগুলি দেখায় কীভাবে Elm এ সহজ ইউনিট এবং ফাজ টেস্ট লিখতে এবং চালাতে হয়, `elm-explorations/test` প্যাকেজ ব্যবহার করে। টেস্টিং হল উন্নয়ন প্রক্রিয়ার একটি জীবন্ত অংশ, যা নিশ্চিত করে যে আপনার Elm অ্যাপ্লিকেশনগুলি নির্ভরযোগ্য এবং উচ্চ মানের থাকে।
