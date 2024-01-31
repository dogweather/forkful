---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:35:59.352626-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक स्ट्रिंग से डेट पार्स करना यानी की एक टेक्स्ट-आधारित तिथि को प्रोग्राम की समझ में आने वाले फॉर्मेट में बदलना होता है। प्रोग्रामर्स यह इसलिए करते हैं ताकि वे तिथियों को संग्रहित कर सकें, तुलना कर सकें, या तिथियों के साथ कोई गणना कर सकें।

## How to: (कैसे करें:)
Elm में स्ट्रिंग से डेट पार्स करने के लिए आपको एक पैकेज की जरूरत होती है। आइए `justinmimbs/date` पैकेज का इस्तेमाल करके देखते हैं।

```elm
import Date
import Date.Extra.Parse as DateParse

-- स्ट्रिंग से ISO 8601 डेट पार्स करना
parseDate : String -> Maybe Date.Date
parseDate dateString =
    DateParse.fromIsoString dateString

-- उदाहरण
parseResult : Maybe Date.Date
parseResult = parseDate "2021-03-14"

```

यदि आप `parseResult` की वैल्यू देखें तो `Just <डेट>` मिलेगी, अगर पार्सिंग सफल होती है, अन्यथा `Nothing` मिलेगी।

## Deep Dive (गहराई से जानकारी)
डेट पार्सिंग लंबे समय से एक जटिल मुद्दा रहा है, खासकर विभिन्न फॉर्मेट्स और लोकेल के कारण। Elm में, `justinmimbs/date` जैसे पैकेज इस समस्या के समाधान में मदद करते हैं। इसके अलावा, `elm/time` कोर पैकेज भी है जो बेसिक डेट और टाइम फंक्शनलिटी प्रदान करता है।

इम्प्लीमेंटेशन डिटेल्स में, पार्सिंग आमतौर पर स्ट्रिंग पैटर्न मैचिंग और डेटा कन्वर्शन पर निर्भर करती है। Elm की स्ट्रिक्ट टाइप सिस्टम गलतियों को कम करती है।

## See Also (और देखें):
- [`justinmimbs/date` पैकेज](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- [Elm `getTime` फंक्शन](https://package.elm-lang.org/packages/elm/time/latest/Time#now)
- [ISO 8601 स्टैंडर्ड](https://www.iso.org/iso-8601-date-and-time-format.html)
- [`elm/time` कोर पैकेज](https://package.elm-lang.org/packages/elm/time/latest/)
