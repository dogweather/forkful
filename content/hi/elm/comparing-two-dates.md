---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:17.190221-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

दो तारीखों की तुलना करना मतलब है यह पहचानना कि कौन सी तारीख पहले है, बाद में है या दोनों बराबर हैं। प्रोग्रामर इसे इवेंट्स, बुकिंग सिस्टम और समय-संबंधित गणनाओं को संभालने के लिए करते हैं।

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
