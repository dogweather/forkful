---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Elm: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वर्तमान तिथि को प्राप्त करना आमतौर पर विकासकों के लिए ऐसे समस्याओं को हल करने का एक भाग है जो तिथि और समय से संबंधित हो सकते हैं। आमतौर पर, यह स्थानीय प्रोग्रामिंग भाषाओं में अत्यधिक संयोजित और भाषा थोड़ी समझने में आसान होता है।

## कैसे करें:
सबसे पहले, हमें Date और Time के लिए एक पैकेज स्थापित करने की आवश्यकता होगी। फिर, आप Date.now फंक्शन का उपयोग करके वर्तमान तिथि और समय को प्राप्त कर सकते हैं।

```Elm
import Date exposing (now)

currentTime : Date
currentTime = now
```

आप उपरोक्त कोड को चला कर, आपको कुछ इस तरह की तिथि देखने को मिलेगी: 
```Elm
[hour = 19, minute = 45, second = 25, millisecond = 244, weekDay = Tuesday, day = 3, month = 10, year = 2021]
```

आप अपनी आवश्यकताओं के अनुसार अन्य Date वैल्यूज भी उपयोग कर सकते हैं, जैसे Date.now आपको सभी मौजूदा समय जोन से समय को प्राप्त करने की अनुमति देता है।

## गहराई पर जाइए:
तिथि को प्राप्त करने के लिए एक और तरीका है Date.Extra नामक पैकेज का उपयोग करना। यह पैकेज Date पैकेज के साथ ऐसी वैश्विक तिथियों के समय और Date संबंधित फंक्शनों को जोड़ता है जो आपको अपनी आवश्यकताओं के अनुसार विभिन्न भाषाओं में उपलब्ध कराता है। इसके अलावा, Moment नामक एक दूसरा पैकेज भी है जो तिथि और समय के साथ काम करने का अनुमति देता है।

## और देखें:
- [Date पैकेज डॉक्यूमेंटेशन](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Date.Extra पैकेज डॉक्यूमेंटेशन](https://package.elm-lang.org/packages/elm-community/date-extra/latest/)
- [Moment पैकेज डॉक्यूमेंटेशन](https://package.elm-lang.org/packages/justinmimbs/elm-moment/latest/)