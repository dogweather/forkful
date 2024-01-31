---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:14:41.409871-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)

वर्तमान तारीख पाना यह सुनिश्चित करता है कि आप कार्यक्रम में आज की तारीख का इस्तेमाल कर सकें। कोडर्स इसे फीचर्स जैसे कैलेंडर, टाइम्लाइन्स और लॉगिंग में इस्तेमाल करते हैं।

## How to: (कैसे करें:)

Elm में वर्तमान तारीख पाने के लिए Time मॉड्यूल का उपयोग होता है। यहां एक साधारण उदाहरण दिखाया गया है:

```Elm
import Time exposing (Posix)
import Task exposing (Task)

-- सर्वर से वर्तमान समय प्राप्त करें
getCurrentTime : Task x Posix
getCurrentTime =
    Time.now

-- Posix समय को तारीख में बदलें
toDateString : Posix -> String
toDateString posix =
    Time.posixToMillis posix
        |> fromMillis
        |> (\date -> date.year ++ "-" ++ date.month ++ "-" ++ date.day)

-- उदाहरण का उपयोग
main =
    getCurrentTime
        |> Task.perform (always ()) (\posix -> Debug.log "Current Date" (toDateString posix))
```

यह कोड संख्येय प्रारूप (Posix) में वर्तमान समय प्राप्त करेगा और इसे एक ह्यूमन-रीडेबल तारीख में परिवर्तित करेगा।

## Deep Dive (गहन जानकारी)

Elm में `Time` मॉड्यूल का निर्माण समय-संबंधित सुविधाओं को सरल और सुरक्षित तरीके से हासिल करने के लिए किया गया था। `Time.now` एक Task देता है जिसे perform करने पर, आपको सर्वर पर वर्तमान UTC समय मिलता है। यह समय `Posix` प्रारूप में होता है, जिसे मिलीसेकैंड्स या ह्यूमन-रीडेबल फॉर्मैट में परिवर्तित किया जा सकता है।

विकल्पों की बात करें तो, अन्य प्रोग्रामिंग भाषाओं में समय को पाने के और भी तरीके हो सकते हैं। जैसे कि, JavaScript में `new Date()` का उपयोग होता है, जबकि Python में `datetime.datetime.now()` का। Elm के पहले के संस्करणों में समय को प्राप्त करने के लिए सीधे पॉजिक्स मानों का उपयोग होता था, जिसे अब Task के साथ किया जाता है ताकि साइड इफेक्ट्स को संभाला जा सके।

## See Also (देखें भी)

- Elm के `Time` मॉड्यूल की आधिकारिक दस्तावेज़ीकरण: [Elm Time Module](https://package.elm-lang.org/packages/elm/time/latest/Time)
- Elm भाषा का आधिकारिक गाइड: [Elm Guide](https://guide.elm-lang.org/)
- Elm के साथ कार्य करने के लिए संदर्भित प्रोजेक्ट्स: [Awesome Elm](https://github.com/sporto/awesome-elm)
