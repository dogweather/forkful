---
date: 2024-01-20 17:31:04.550803-07:00
description: "\u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E\
  \ \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u092D\u0935\u093F\u0937\u094D\
  \u092F \u092F\u093E \u0905\u0924\u0940\u0924 \u092E\u0947\u0902 \u0915\u093F\u0938\
  \u0940 \u0916\u093E\u0938 \u0926\u093F\u0928 \u0915\u0940 \u0924\u0932\u093E\u0936\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0947 \u0908\u0935\u0947\u0902\u091F\u094D\u0938, \u0930\u093F\u092E\u093E\u0907\
  \u0902\u0921\u0930\u094D\u0938, \u0914\u0930 \u091F\u093E\u0907\u092E-\u092C\u093E\
  \u0909\u0902\u0921 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u094B \u0939\
  \u0948\u0902\u0921\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:52.209709-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u092D\u0935\u093F\u0937\u094D\u092F\
  \ \u092F\u093E \u0905\u0924\u0940\u0924 \u092E\u0947\u0902 \u0915\u093F\u0938\u0940\
  \ \u0916\u093E\u0938 \u0926\u093F\u0928 \u0915\u0940 \u0924\u0932\u093E\u0936\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0908\u0935\u0947\u0902\u091F\u094D\u0938, \u0930\u093F\u092E\u093E\u0907\u0902\
  \u0921\u0930\u094D\u0938, \u0914\u0930 \u091F\u093E\u0907\u092E-\u092C\u093E\u0909\
  \u0902\u0921 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u094B \u0939\u0948\
  \u0902\u0921\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
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
