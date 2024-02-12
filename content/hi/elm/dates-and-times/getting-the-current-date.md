---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/elm/getting-the-current-date.md
date:                  2024-02-03T19:10:18.175563-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Elm में वर्तमान तिथि प्राप्त करने का मतलब है सिस्टम से वर्तमान कैलेंडर तिथि को लाना। हम इसे घटनाओं को टाइमस्टैम्प करने, कार्यों की अनुसूची बनाने, या अवधि को ट्रैक करने के लिए करते हैं।

## कैसे करें:
Elm `Time` मॉड्यूल के साथ तिथियों को संभालता है। आप वर्तमान समय को एक POSIX टाइमस्टैम्प के रूप में प्राप्त करेंगे, फिर उसे तिथि में परिवर्तित करेंगे।

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- POSIX समय को एक तारीख रिकॉर्ड में बदलें
                date = Time.toDate posixTime
            in
            -- यहाँ पर अपने मॉडल को उसी अनुसार अपडेट करें
            ({ model | date = date }, Cmd.none)

-- वर्तमान समय प्राप्त करने की प्रक्रिया आरंभ करने के लिए
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- उदाहरण आउटपुट:
-- date { year = 2023, month = Mar, day = 26 }
```

## गहराई में जानकारी
पुरानी वेब भाषाओं में, तिथि प्राप्त करना एक-लाइन कोड है। Elm अलग है। यह साइड-इफेक्ट्स जैसे कि वर्तमान समय प्राप्त करना Elm आर्किटेक्चर के माध्यम से स्पष्ट बनाता है। यह कोड की शुद्धता और रखरखाव को प्रोत्साहित करता है।

विकल्पों में तृतीय-पक्ष पैकेज का उपयोग करना या अपने सर्वर कोड में तिथियों को संभालना और उन्हें Elm के लिए फ्लैग्स या पोर्ट्स के माध्यम से पास करना शामिल है।

कार्यान्वयन के लिहाज से, Elm का `Time.now` POSIX टाइमस्टैम्प (Unix epoch के बाद से मिलीसेकंड्स) के रूप में समय प्राप्त करता है। यह समय क्षेत्र से स्वतंत्र है, और आप इसे `Time` मॉड्यूल से फंक्शन्स का उपयोग करके आवश्यकतानुसार प्रारूपित कर सकते हैं।

## और भी देखें
- [Elm Time दस्तावेज़ीकरण](https://package.elm-lang.org/packages/elm/time/latest/)
- [कमांड्स और सब्सक्रिप्शन्स के लिए Elm की मार्गदर्शिका](https://guide.elm-lang.org/effects/)
