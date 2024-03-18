---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:42.645542-06:00
description: "\u09B2\u0997\u09BF\u0982 \u09AE\u09C2\u09B2\u09A4 \u098F\u09AE\u09A8\
  \ \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \u09BE \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09AB\u099F\u0993\u09AF\u09BC\
  \u09CD\u09AF\u09BE\u09B0 \u099A\u09B2\u09BE\u0995\u09BE\u09B2\u09C0\u09A8 \u0998\
  \u099F\u09A8\u09BE\u09AC\u09B2\u09C0 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F\u09C7\u09B0 \u09B0\u09C7\u0995\u09B0\u09CD\
  \u09A1 \u09B0\u09BE\u0996\u09C7, \u098F\u0995\u09C7 \u09B8\u09AB\u099F\u0993\u09AF\
  \u09BC\u09CD\u09AF\u09BE\u09B0\u09C7\u09B0 \u09A1\u09BE\u09AF\u09BC\u09C7\u09B0\u09BF\
  \ \u09AE\u09A8\u09C7 \u0995\u09B0\u09C1\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09BF \u0998\u099F\u099B\
  \u09C7\u2026"
lastmod: '2024-03-17T18:47:43.955194-06:00'
model: gpt-4-0125-preview
summary: "\u09B2\u0997\u09BF\u0982 \u09AE\u09C2\u09B2\u09A4 \u098F\u09AE\u09A8 \u098F\
  \u0995\u099F\u09BF \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\
  \ \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09AB\u099F\u0993\u09AF\u09BC\u09CD\
  \u09AF\u09BE\u09B0 \u099A\u09B2\u09BE\u0995\u09BE\u09B2\u09C0\u09A8 \u0998\u099F\
  \u09A8\u09BE\u09AC\u09B2\u09C0 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE \u0986\
  \u0989\u099F\u09AA\u09C1\u099F\u09C7\u09B0 \u09B0\u09C7\u0995\u09B0\u09CD\u09A1\
  \ \u09B0\u09BE\u0996\u09C7, \u098F\u0995\u09C7 \u09B8\u09AB\u099F\u0993\u09AF\u09BC\
  \u09CD\u09AF\u09BE\u09B0\u09C7\u09B0 \u09A1\u09BE\u09AF\u09BC\u09C7\u09B0\u09BF\
  \ \u09AE\u09A8\u09C7 \u0995\u09B0\u09C1\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09BF \u0998\u099F\u099B\
  \u09C7\u2026"
title: "\u09B2\u0997\u09BF\u0982"
---

{{< edit_this_page >}}

## কি এবং কেন?
লগিং মূলত এমন একটি প্রক্রিয়া যা একটি সফটওয়্যার চলাকালীন ঘটনাবলী এবং ডেটা আউটপুটের রেকর্ড রাখে, একে সফটওয়্যারের ডায়েরি মনে করুন। প্রোগ্রামাররা কি ঘটছে তা ট্র্যাক রাখার জন্য লগিং ব্যবহার করে - এটি সমস্যা ডিবাগ করা, রিয়েল-টাইমে সিস্টেম আচরণ মনিটর করা এবং পারফরম্যান্স অপ্টিমাইজেশন বা অডিটের জন্য অতীত কার্যক্রম বিশ্লেষণ করা অমূল্য।

## কিভাবে:
Elm এর আর্কিটেকচার সরাসরি লগিংযের মত সাইড ইফেক্টস সাপোর্ট করে না—আপনি তাদের কমান্ডসের মাধ্যমে সামলান, যা আপনার অ্যাপ্লিকেশনের আর্কিটেকচারের একটি অংশ। শিক্ষামূলক উদ্দেশ্যে, চলুন দেখা যাক কিভাবে আপনি পোর্টের মাধ্যমে জাভাস্ক্রিপ্টে বার্তা পাঠিয়ে লগিং সিমুলেট করতে পারেন।

প্রথমে, আপনি একটি পোর্ট মডিউল সংজ্ঞায়িত করবেন:

```Elm
port module Logger exposing (..)

-- জাভাস্ক্রিপ্টে লগ প্রেরণের জন্য একটি পোর্ট নির্ধারণ করুন
port log : String -> Cmd msg
```

আপনার `Main.elm` এ, আপনি একটি লগ বার্তা প্রেরণের জন্য `log` পোর্ট ব্যবহার করবেন:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- আপনার মডেলে এখানে কিছু আপডেট
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- এখানে অন্যান্য মডেল আপডেটস
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

জাভাস্ক্রিপ্ট দিকে, আপনি আসন্ন লগ বার্তা হ্যান্ডেল করার জন্য `log` পোর্টে সাবস্ক্রাইব করতে পারেন:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

জাভাস্ক্রিপ্ট কনসোলে নমুনা আউটপুট হবে:

```
AnEvent occurred.
AnotherEvent occurred.
```

## গভীর ডুব
প্রথাগতভাবে, পাইথন বা জাভা এর মতো ভাষাগুলিতে, লগিং একটি লগিং লাইব্রেরি ব্যবহার করে করা হয়, যা বিভিন্ন লেভেলে যেমন ডিবাগ, ইনফো, ওয়ার্নিং, এরর, এবং ক্রিটিকাল বার্তা লগ করার জন্য সরাসরি API প্রদান করে।

Elm, এর শুদ্ধতা এবং অপরিবর্তনীয়তার উপর মনোনিবেশ সহ, এই ধরনের সরাসরি লগিং প্রদান করে না, যেহেতু যেকোনো ধরনের IO বা সাইড ইফেক্ট Elm আর্কিটেকচারের মাধ্যমে আলাদাভাবে ম্যানেজ করা হয়।

Elm এ পূর্ণ-বৈশিষ্ট্যযুক্ত লগিং প্রয়োজন হলে, সাধারণত আপনি বাইরের জাভাস্ক্রিপ্ট টুলসে নির্ভর করেন। উপরে দেখানো পোর্টস, এই টুলগুলির সাথে সেতুর কাজ করে। ডিবাগ মডিউল অন্য একটি অপশন, কিন্তু এটি কেবল ডেভেলপমেন্ট ব্যবহার এবং প্রোডাকশন লগিংয়ের জন্য নয়।

পোর্টের পাশাপাশি, প্রোগ্রামাররা প্রায়ই Elmের কম্পাইলার বার্তা এবং রানটাইম ডিবাগিং সুবিধাগুলি, যেমন `Debug.log`, তাদের কোডে মান ট্রেস করার জন্য ব্যবহার করেন। এটি একটি এক্সপ্রেশনকে র্যাপ করে এবং এর আউটপুটকে কনসোলে লগ করে এমনভাবে:

```Elm
view model =
    Debug.log "Model Debug" model
    -- আপনার ভিউ কোড এখানে
```

তবে এটিও প্রোডাকশনের জন্য নয়। elm-logger মতো টুলগুলি পোর্টের উপর কিছু অ্যাবস্ট্রাকশন প্রদান করে লগিংয়ের জন্য, যদিও এগুলি প্রোডাকশনের চেয়ে ডেভেলপমেন্টের জন্য বেশি।

## আরও দেখুন
- Elm পোর্টস: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm এ লগিং নিয়ে আলোচনা: https://discourse.elm-lang.org/t/elm-and-logging/546
- জাভাস্ক্রিপ্ট কনসোল API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger প্যাকেজ: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
