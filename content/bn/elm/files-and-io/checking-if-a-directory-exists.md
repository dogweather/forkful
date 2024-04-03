---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:59.438921-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm \u098F\u0995\u099F\u09BF \u09AB\
  \u09CD\u09B0\u09A8\u09CD\u099F-\u098F\u09A8\u09CD\u09A1 \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AD\
  \u09BE\u09B7\u09BE, \u09A4\u09BE\u0987 \u098F\u099F\u09BF \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u09AA\u09BE\u09AF\
  \u09BC \u09A8\u09BE\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3\u09A4 \u098F\u0995\u099F\u09BF \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1 JavaScript \u098F \u09AC\u09CD\u09AF\u09BE\u0995-\u098F\u09A8\u09CD\
  \u09A1\u2026"
lastmod: '2024-03-17T18:47:43.963236-06:00'
model: gpt-4-0125-preview
summary: "Elm \u098F\u0995\u099F\u09BF \u09AB\u09CD\u09B0\u09A8\u09CD\u099F-\u098F\
  \u09A8\u09CD\u09A1 \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE, \u09A4\u09BE\u0987\
  \ \u098F\u099F\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\
  \u09CD\u09B8\u09C7\u09B8 \u09AA\u09BE\u09AF\u09BC \u09A8\u09BE\u0964 \u09A4\u09AC\
  \u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u098F\
  \u0995\u099F\u09BF \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 JavaScript \u098F \u09AC\
  \u09CD\u09AF\u09BE\u0995-\u098F\u09A8\u09CD\u09A1 \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BF\u09B8\u09C7 \u09AA\u09BE\u09A0\u09BE\u09A8\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 Elm \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u0985\u09CD\u09AF\u09BE\u0995\u09B6\u09A8 \u0997\u09A0\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
Elm একটি ফ্রন্ট-এন্ড ওয়েব প্রোগ্রামিং ভাষা, তাই এটি সরাসরি ফাইল সিস্টেমে অ্যাক্সেস পায় না। তবে, আপনি সাধারণত একটি কমান্ড JavaScript এ ব্যাক-এন্ড সার্ভিসে পাঠান। এখানে কিভাবে Elm এর সাথে এমন একটি ইন্টারঅ্যাকশন গঠন করতে পারেন:

```elm
port module Main exposing (..)

-- JavaScript এর সাথে কথা বলার জন্য একটি পোর্ট ডিফাইন করুন
port checkDir : String -> Cmd msg

-- উদাহরণ ব্যবহার
checkDirectory : String -> Cmd Msg
checkDirectory dir =
    checkDir dir
```

তারপর, আপনার JavaScript এ:

```javascript
app.ports.checkDir.subscribe(function(dir) {
    var exists = fs.existsSync(dir); // এটি Node's 'fs' মডিউল ব্যবহার করে ডিরেক্টরি চেক করে
    app.ports.dirExists.send(exists);
});
```

Elm এ ফিরে, প্রতিক্রিয়া হ্যান্ডেল করুন:

```elm
port dirExists : (Bool -> msg) -> Sub msg

type Msg = DirExists Bool

subscriptions : Model -> Sub Msg
subscriptions model =
    dirExists DirExists
```

নোট: এটি JavaScript এ পোর্টস এবং উপযুক্ত ব্যাক-এন্ড হ্যান্ডলিং সেট আপ করা প্রয়োজন।

## গভীর ডুব
Elm এর ব্রাউজার-সীমাবদ্ধ পরিবেশ এর অর্থ হল এটি সরাসরি ফাইল সিস্টেমে অ্যাক্সেস পেতে পারে না, Node.js এর মতো। ঐতিহাসিকভাবে, সার্ভার-সাইড ভাষা এবং Node.js ফাইল সিস্টেম অ্যাক্সেসের জন্য কার্যকারিতা প্রদান করে, যেখানে ব্রাউজার ভাষা ফাইল ম্যানেজ করতে সার্ভার API গুলির উপর নির্ভর করে। Elm এর কঠিন টাইপ সিস্টেম সাইড ইফেক্ট যেমন ইনপুট/আউটপুট অপারেশন নেটিভভাবে ম্যানেজ করে না; পরিবর্তে, এটি JavaScript ইন্টারঅ্প এর জন্য পোর্টস ব্্যবহার করে। যদিও Elm নিজে চেক করতে পারে না কোন ডিরেক্টরি আছে কিনা, Elm কে ব্যাক-এন্ড সার্ভিসের মাধ্যমে পোর্টস এর মাধ্্যমে ব্্যবহার করে ওয়েব অ্যাপ্লিকেশনে এই কার্যকারিতা প্রদান করা যায়।

Node.js পরিবেশে বিকল্প অন্তর্ভুক্ত `fs.existsSync` বা `fs.access` পদ্ধতি। Elm এর জন্য, সার্ভার-সাইড Elm যেমন `elm-serverless` ভাবনা করুন, যা ক্লা্ইন্্ট-সাইড Elm এর চেয়ে সরাসরি ফাইল অপারেশন হ্যান্ডেল করতে পারে।

বাস্তবায়নের দিক থেকে, একবার আপনি আপনার পোর্টস সেট আপ করে ফেললে, আপনার Elm অ্যাপ JavaScript এ বার্তা পাঠায় যা ফাইল সিস্টেম চেক করে। JavaScript তারপর ফলাফল Elm এ পাঠায়। এটি Elm এর ফ্রন্ট-এন্ড কোডকে পার্শ্ব প্রভাব থেকে মুক্ত এবং পরিষ্কার রাখে, এর স্থাপত্য নীতিগুলি বজায় রেখে।

## দেখুন এছাড়াও
- Elm অফিসিয়াল গাইড পোর্টসে: https://guide.elm-lang.org/interop/ports.html
- Node.js `fs` মডিউল ডকুমেন্টেশন: https://nodejs.org/api/fs.html
- সার্ভার-সাইড Elm ইন্্টার্্যাক্্শনের জন্্য elm-serverless: https://package.elm-lang.org/packages/ktonon/elm-serverless/latest/
