---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:45.350729-06:00
description: "TOML, \u09AF\u09BE Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\
  \u0982\u0995\u09CD\u09B7\u09C7\u09AA, \u098F\u0995\u099F\u09BF \u09A1\u09BE\u099F\
  \u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8 \u09AD\u09BE\u09B7\u09BE\u0964 Elm \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u0995\u09C7 \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09A1\u09BE\u099F\u09BE \u09AA\
  \u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.973774-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u09AF\u09BE Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\u0982\
  \u0995\u09CD\u09B7\u09C7\u09AA, \u098F\u0995\u099F\u09BF \u09A1\u09BE\u099F\u09BE\
  \ \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8 \u09AD\u09BE\u09B7\u09BE\u0964 Elm \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u0995\u09C7 \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09A1\u09BE\u099F\u09BE \u09AA\
  \u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 \u098F\
  \u099F\u09BF \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09B9\u099C \u098F\u09AC\u0982 \u0985\u09CD\
  \u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u0995\u09C0-\u09AE\u09BE\
  \u09A8 \u099C\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B8\u09C1\
  \u099A\u09BE\u09B0\u09C1\u09AD\u09BE\u09AC\u09C7 \u09AE\u09BE\u09A8\u099A\u09BF\u09A4\
  \u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
Elm-এ কোনো নির্মিত TOML পার্সার নেই, তবে আপনি JavaScript এর সাথে ইন্টারঅপ করতে পারেন বা একটি কমিউনিটি প্যাকেজ ব্যবহার করতে পারেন। এখানে কিভাবে আপনি একটি কাল্পনিক `elm-toml` প্যাকেজ ব্যবহার করে TOML পার্স করতে পারেন:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

নির্দিষ্ট মানগুলি ডিকোড করার জন্য:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

`port` এর জন্য নমুনা আউটপুট যদি ডিকোডিং সফল হয় তবে `Ok 8080` হতে পারে।

## গভীর ডুব
TOML টম প্রেস্টন-ওয়ার্নার, GitHub এর সহ-প্রতিষ্ঠাতা দ্বারা তৈরি করা হয়েছিল, যা কনফিগারেশন ফাইলের জন্য একটি সহজ ভাষা হিসেবে। এটি YAML এবং JSON এর সাথে প্রতিযোগিতা করে; TOML এর সিনট্যাক্স উভয় পৃথিবীর সেরা দিকগুলির লক্ষ্য নিয়েছে এমন একটি ফোকাস নিয়ে যা মানুষের পড়া এবং লেখা উভয়ের জন্যই সহজ।

Elm এ, TOML কে হ্যান্ডেল করার জন্য, আপনাকে সাধারণত JavaScript ইন্টারঅপ এর মাধ্যমে যেতে হয়, যা কিছুটা ঝামেলাপূর্ণ হতে পারে। সৌভাগ্যক্রমে, Elm কমিউনিটি সম্পদশালী, এবং বেশ কয়েকটি থার্ড-পার্টি প্যাকেজ বিদ্যমান। কাল্পনিক `elm-toml` প্যাকেজটি সম্ভবত Elm এর `Port` ব্যবহার করে একটি JavaScript TOML পার্সার এর সাথে কথা বলবে বা সরাসরি Elm এ পার্সিং বাস্তবায়ন করবে।

Elm এর প্রধান চ্যালেঞ্জ হচ্ছে এটি সর্বকিছুকে স্ট্যাটিক্যালি টাইপ করে, তাই আপনাকে TOML এর বিভিন্ন ডাটা স্ট্রাকচার হ্যান্ডেল করার জন্য কাস্টম ডিকোডার লিখতে হবে, যা কিছুটা বাচাল হতে পারে কিন্তু সুরক্ষা যোগাযোগ করে।

## আরও দেখুন
TOML নিজের উপর স্পেসিফিকেশন এবং আরও তথ্যের জন্য, দেখুন [TOML](https://toml.io)।
যদি আপনি Elm এবং JavaScript ইন্টারঅপ এর উপর প্র্যাকটিক্যাল পদ্ধতি খুঁজছেন, তবে অফিসিয়াল গাইড দিয়ে শুরু করুন: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)।
কমিউনিটি প্যাকেজগুলি খুঁজতে বা অবদান রাখতে, ব্রাউজ করুন [Elm Packages](https://package.elm-lang.org/)।
