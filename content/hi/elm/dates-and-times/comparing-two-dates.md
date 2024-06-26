---
date: 2024-01-20 17:33:17.190221-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\
  \u0947\u0902 \u0924\u093E\u0930\u0940\u0916\u0947\u0902 \u0915\u0940 \u0924\u0941\
  \u0932\u0928\u093E `Date` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u0947\
  \ `compare` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u0947 \u091C\u0930\u093F\
  \u090F \u0915\u0940 \u091C\u093E\u0924\u0940 \u0939\u0948, \u091C\u094B `Basics.Order`\
  \ \u091F\u093E\u0907\u092A (LT, GT, EQ) \u0935\u093E\u092A\u0938 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964\u2026"
lastmod: '2024-04-05T22:51:06.897376-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elm \u092E\u0947\u0902\
  \ \u0924\u093E\u0930\u0940\u0916\u0947\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\
  \u093E `Date` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u0947 `compare`\
  \ \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u0947 \u091C\u0930\u093F\u090F \u0915\
  \u0940 \u091C\u093E\u0924\u0940 \u0939\u0948, \u091C\u094B `Basics.Order` \u091F\
  \u093E\u0907\u092A (LT, GT, EQ) \u0935\u093E\u092A\u0938 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948\u0964 \u0907\u0924\u093F\u0939\u093E\u0938 \u092E\u0947\u0902, Elm\
  \ 0.19 \u0938\u0947 \u092A\u0939\u0932\u0947 \u0924\u093E\u0930\u0940\u0916\u094B\
  \u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0915\u0947 \u0932\u093F\u090F\
  \ \u0905\u0932\u0917 \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0925\u0947\
  , \u092A\u0930 \u0905\u092C `Date` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u092E\
  \u093E\u0928\u0915 \u0909\u092A\u0915\u0930\u0923\u094B\u0902 \u0915\u0947 \u0938\
  \u093E\u0925 \u0906\u0924\u093E \u0939\u0948\u0964 \u0935\u0948\u0915\u0932\u094D\
  \u092A\u093F\u0915 \u0930\u0942\u092A \u0938\u0947, \u0906\u092A `Time.posix` \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 time stamps \u0915\u0940 \u0938\
  \u0940\u0927\u0940 \u0924\u0941\u0932\u0928\u093E \u092D\u0940 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0907\u0938\u0915\u093E \u0915\u093E\
  \u0930\u094D\u092F\u093E\u0928\u094D\u0935\u092F\u0928 `time` \u092A\u0948\u0915\
  \u0947\u091C \u092E\u0947\u0902 \u0939\u094B\u0924\u093E \u0939\u0948, \u091C\u093F\
  \u0938\u0947 `core` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\
  \u0947\u0902 \u0906\u092A\u0915\u094B \u0906\u092F\u093E\u0924 \u0915\u0930\u0928\
  \u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## How to: (कैसे करें:)
```Elm
import Time
import Date exposing (Date)

compareDates : Date -> Date -> Basics.Order
compareDates date1 date2 =
    Date.compare date1 date2

-- उदाहरण प्रयोग
date1 : Date
date1 =
    Date.fromIsoString "2023-02-20" |> Result.withDefault Date.zero

date2 : Date
date2 =
    Date.fromIsoString "2023-02-21" |> Result.withDefault Date.zero

-- compareDates का इस्तेमाल करके तुलना करना
result : String
result =
    case compareDates date1 date2 of
        LT -> "Date1 पहले है"
        GT -> "Date1 बाद में है"
        EQ -> "दोनों तारीखें बराबर हैं"

-- आउटपुट: "Date1 पहले है"
```

## Deep Dive (गहराई से जानकारी)
Elm में तारीखें की तुलना `Date` मॉड्यूल के `compare` फंक्शन के जरिए की जाती है, जो `Basics.Order` टाइप (LT, GT, EQ) वापस करता है। इतिहास में, Elm 0.19 से पहले तारीखों की तुलना के लिए अलग फंक्शन्स थे, पर अब `Date` मॉड्यूल मानक उपकरणों के साथ आता है। वैकल्पिक रूप से, आप `Time.posix` का उपयोग कर time stamps की सीधी तुलना भी कर सकते हैं। इसका कार्यान्वयन `time` पैकेज में होता है, जिसे `core` लाइब्रेरी में आपको आयात करना होता है।

## See Also (देखने योग्य अन्य स्रोत)
- Elm `Date` के डॉक्युमेंटेशन: [packages.elm-lang.org/packages/justinmimbs/date/latest/](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Elm `Time` के डॉक्युमेंटेशन: [package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Guide on Time and Dates: [guide.elm-lang.org/effects/time.html](https://guide.elm-lang.org/effects/time.html)
