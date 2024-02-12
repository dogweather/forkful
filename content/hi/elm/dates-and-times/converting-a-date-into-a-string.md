---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases:
- /hi/elm/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:40.470161-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Dates को strings में बदलने का मतलब है उन्हें पढ़ने योग्य फॉर्म में प्रस्तुत करना। Programmers इसे करते हैं user interface में तारीखें दिखाने या logs में तारीख को सहेजने के लिए।

## How to: (कैसे करें:)
Elm में, हम `Date` library का उपयोग करके dates को strings में बदलते हैं।

```Elm
import Date
import Time exposing (Posix)

-- एक पोज़िक्स टाइम को डेट स्ट्रिंग में कन्वर्ट करने का उदाहरण
toDateString : Posix -> String
toDateString posix =
    let
        date = Date.fromPosix posix
    in
    Date.toIsoString date

-- उपयोग का उदाहरण
exampleDate : Posix
exampleDate = 
    Time.millisToPosix 1617976541000

-- आउटपुट
toDateString exampleDate
-- "2021-04-09T12:49:01Z"
```
यह दिखाता है कि `exampleDate` जो `Posix` time है, को ISO 8601 फॉर्मेट में कैसे बदला जा सकता है।

## Deep Dive (गहराई से जानकारी)
इतिहास में पहले, dates को अलग-अलग फॉर्मेट्स में मैनुअली हेरफेर किया जाता था। अब, `Date` library जैसे टूल्स के साथ, हम आसानी से और तेजी से इसे कर पाते हैं। Elm की `Date` library अलग-अलग time zones और cultures के तारीख फॉर्मेट्स को संभाल सकती है। 

विकल्प के रूप में, कस्टम फॉर्मेटिंग के लिए आप `elm-format-date` जैसे पैकेजों का भी इस्तेमाल कर सकते हैं।

Elm में डेट को स्ट्रिंग में बदलना, पोर्टेबल (portable) और ढांचे में सुसंगत (consistent) होता है, जिससे यह सुनिश्चित होता है कि एप्लिकेशन अलग-अलग डिवाइसेज़ और प्लेटफ़ॉर्म्स पर निर्भर नहीं होता।

## See Also (यह भी देखें)
और जानकारी और सीखने के लिए, नीचे दिए गए संसाधनों को देखें:

- [Elm Date Documentation](https://package.elm-lang.org/packages/elm/time/latest/Date) - Elm के `Date` फंक्शन्स के बारे में सभी जानकारी।
- [Elm Time Library](https://package.elm-lang.org/packages/elm/time/latest/) - Elm की टाइम हैंडलिंग क्षमताएं।
- [ISO 8601 Wikipedia](https://en.wikipedia.org/wiki/ISO_8601) - ISO 8601 स्टैंडर्ड के बारे में और विवरण।
- [Elm Guide](https://guide.elm-lang.org/) - Elm लैंग्वेज को सीखने के लिए आधिकारिक गाइड।
- [elm-format-date](https://package.elm-lang.org/packages/ryannhg/elm-date-format/latest/) - डेट को अलग-अलग फॉर्मेट्स में कन्वर्ट करने के लिए एक Elm पैकेज।
