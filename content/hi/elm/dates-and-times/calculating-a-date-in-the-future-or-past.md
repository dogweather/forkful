---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/elm/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:04.550803-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

तारीख की गणना का मतलब है भविष्य या अतीत में किसी खास दिन की तलाश। प्रोग्रामर इसे ईवेंट्स, रिमाइंडर्स, और टाइम-बाउंड कार्यों को हैंडल करने के लिए करते हैं।

## कैसे करें? (How to:)

Elm में एक date को manipulate करने के लिए हम `Time` package का इस्तेमाल कर सकते हैं। यहाँ एक सिंपल उदाहरण है:

```Elm
import Time exposing (..)
import Date exposing (..)

addDays : Int -> Date -> Date
addDays days date =
    let
        duration = days * 86400000 -- Milliseconds in a day
    in
        Date.fromTime (Time.posixToMillis (Date.toTime date) + duration)
```

सैंपल आउटपुट कुछ इस तरह होगा:

```Elm
> current = Date.fromTime (Time.millisToPosix 0)
<Date 1970-01-01>
> addDays 10 current
<Date 1970-01-11>
```

## गहराई से समझ (Deep Dive)

इतिहास में, तारीख की गणना हमेशा कैलेंडर सिस्टम्स और टाइमज़ोन्स के जटिल नियमों के कारण चुनौतीपूर्ण रही है। Elm में `Date` और `Time` पैकेज सरलीकृत इंटरफेसेज प्रदान करते हैं लेकिन पूरी तरह से बारीकियों को संभालने के लिए नहीं। उदाहरण के लिए, लीप इयर्स और डेलाइट सेविंग्स टाइम जैसे इश्यूज पर खास ध्यान देना पड़ता है। `moment.js` एक JavaScript लाइब्रेरी है जो इन जटिलताओं को हैंडल करती है, और Elm प्रोजेक्ट में JavaScript इंटरऑपरेबिलिटी का इस्तेमाल करके इसका लाभ उठाया जा सकता है।

## देखें इसे भी (See Also)

- [Elm Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date Documentation](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- [Elm Guide on Time](https://guide.elm-lang.org/effects/time.html)
- [JavaScript Interop in Elm](https://guide.elm-lang.org/interop/)
