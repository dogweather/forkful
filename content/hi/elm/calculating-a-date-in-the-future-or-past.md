---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Elm: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य या अतीत में तारीख की गणना क्या होती है और प्रोग्रामर्स इसे क्यों करते हैं? भविष्य या अतीत की दिनांक गणना, किसी विशेष दिनांक से कुछ दिनों को जोड़ने या घटाने का कार्य होता है।  प्रोग्रामर्स इसे तारीख और समय से संबंधित कार्यों को संचालित करने के लिये करते हैं।

## कैसे करें:
यहां हमने एल्म स्क्रिप्ट के माध्यम से एक दिनांक को गणना किया है:

```Elm
import Time exposing (..)
import Task
import Task.Extra as Task exposing (..)
import Date.Extra as Date exposing (..)

doCalculation : Time.Posix -> Time.Zone -> Maybe Date
doCalculation posix datezone = 
    Date.fromPosix datezone posix
    |> Maybe.map (\date -> Date.add Date.Day 7 date )

main =
    Task.attempt always 
    <| Task.map3 doCalculation (Time.now) (Task.succeed Time.utc)
```
संचलन के परिणाम स्वरूप समय स्थिति के 7 दिन बाद की तारीख दिखाई देगी। 

## गहरी दिविंग
यदि आप भविष्य या अतीत में तारीख की गणना को अधिक संदर्भ में देखना चाहते हैं, पुराने दौर में तारीख की गणना का उपयोग केवल कैलेंडर को चालने के लिए होता था। लेकिन आज के समय में, इसका उपयोग सटीक समय प्रबंधन, प्रावधान और सूचनाबद्धता के लिये किया जाता है। एल्म में तारीख की गणना के लिए कुछ विकल्पों में से एक है "Date.Extra" पैकेज जो तारीखों की संचालन और गणना में मदद करता है। 

## उपयोगी लिंक्स
ये कुछ उपयोगी स्रोत हैं जो अतिरिक्त जानकारी प्रदान करते हैं:
- [Elm के लिए अधिकारिक डॉक्यूमेंटेशन](https://guide.elm-lang.org)
- [पुराने दौर में तारीख की गणना](https://en.wikipedia.org/wiki/Date_and_time_notation_in_Europe)