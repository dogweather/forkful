---
date: 2024-01-20 17:36:40.470161-07:00
description: "Dates \u0915\u094B strings \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0909\u0928\u094D\u0939\
  \u0947\u0902 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092B\
  \u0949\u0930\u094D\u092E \u092E\u0947\u0902 \u092A\u094D\u0930\u0938\u094D\u0924\
  \u0941\u0924 \u0915\u0930\u0928\u093E\u0964 Programmers \u0907\u0938\u0947 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902 user interface \u092E\u0947\u0902 \u0924\u093E\
  \u0930\u0940\u0916\u0947\u0902 \u0926\u093F\u0916\u093E\u0928\u0947 \u092F\u093E\
  \ logs \u092E\u0947\u0902\u2026"
lastmod: '2024-03-11T00:14:26.107537-06:00'
model: gpt-4-1106-preview
summary: "Dates \u0915\u094B strings \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947\
  \ \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0909\u0928\u094D\u0939\u0947\
  \u0902 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092B\u0949\
  \u0930\u094D\u092E \u092E\u0947\u0902 \u092A\u094D\u0930\u0938\u094D\u0924\u0941\
  \u0924 \u0915\u0930\u0928\u093E\u0964 Programmers \u0907\u0938\u0947 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 user interface \u092E\u0947\u0902 \u0924\u093E\u0930\
  \u0940\u0916\u0947\u0902 \u0926\u093F\u0916\u093E\u0928\u0947 \u092F\u093E logs\
  \ \u092E\u0947\u0902\u2026"
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
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
