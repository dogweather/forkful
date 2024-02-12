---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- /hi/elm/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:14.871269-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Elm में एक स्ट्रिंग से तिथि पार्स करने का अर्थ है तिथियों और समयों को दर्शाती टेक्स्टुअल जानकारी को Elm के समझने और संचालन करने योग्य प्रारूप में परिवर्तित करना, विशेष रूप से `Date` प्रकार में। यह प्रक्रिया उपयोगकर्ता इनपुट को संभालने, स्थानीय रूप से सही तरीके से तिथियों को प्रदर्शित करने, और तिथि संबंधित गणनाओं को करने के लिए महत्वपूर्ण है, यह सुनिश्चित करते हुए कि आपके Elm अनुप्रयोग समय संबंधित डेटा को बुद्धिमानी से संसाधित कर सकें।

## कैसे करें:
Elm के पास तिथि पार्सिंग के लिए अन्य भाषाओं के समान मजबूत निर्मित क्षमताएं नहीं हैं, प्रमुख रूप से जटिल ऑपरेशनों के लिए Javascript इंटरॉप या लाइब्रेरीज़ पर निर्भर करती हैं। हालांकि, आप बेसिक पार्सिंग के लिए `elm/time` पैकेज का उपयोग कर सकते हैं, और अधिक जटिल आवश्यकताओं के लिए, तृतीय-पक्ष `justinmimbs/date` लाइब्रेरी की सिफारिश व्यापक रूप से की जाती है।

### `elm/time` का उपयोग करके पार्सिंग:
`elm/time` `Time` मॉड्यूल प्रदान करता है, जो आपको मानव-पठनीय तिथियों के बजाय टाइमस्टैम्प्स के साथ काम करने की सुविधा देता है। जबकि यह स्ट्रिंग से सीधे तिथियों को पार्स नहीं करता, आप एक ISO 8601 स्ट्रिंग को POSIX टाइमस्टैम्प में परिवर्तित कर सकते हैं, जिसके साथ आप फिर काम कर सकते हैं।

```elm
import Time exposing (Posix)

-- मान लिजिए आपके पास एक ISO 8601 तिथि स्ट्रिंग है
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- इसे POSIX टाइमस्टैम्प में परिवर्तित करें (यह फंक्शन एक `Result` लौटाता है)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- नमूना आउटपुट: Ok <posix time value>
```

### `justinmimbs/date` का उपयोग करके पार्सिंग:
जटिल पार्सिंग के लिए, जैसे कि नॉन-ISO प्रारूपों से निपटना, `justinmimbs/date` लाइब्रेरी एक शानदार विकल्प है। यहाँ आप इसका उपयोग करके एक कस्टम तिथि स्ट्रिंग को कैसे पार्स कर सकते हैं:

1. सुनिश्चित करें कि आपके पास लाइब्रेरी स्थापित है:

```shell
elm install justinmimbs/date
```

2. कस्टम तिथि प्रारूपों को पार्स करने के लिए `Date.fromString` फंक्शन का उपयोग करें:

```elm
import Date
import Result exposing (Result(..))

-- मान लिजिए आपके पास एक कस्टम तिथि स्ट्रिंग प्रारूप है `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- कस्टम प्रारूप को पार्स करने के लिए फंक्शन
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- नमूना उपयोग
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- नमूना आउटपुट: Ok (Date.fromCalendarDate 2023 Jan 1)
```

इन उदाहरणों में, `Result` प्रकार या तो एक सफल पार्सिंग को encapsulate करता है जो एक तिथि (`Ok`) देता है या एक त्रुटि (`Err`), आपके Elm अनुप्रयोगों में दृढ़ त्रुटि हैंडलिंग को सक्षम करता।
