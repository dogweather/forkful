---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:37:21.257379-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तिथि को स्ट्रिंग में बदलना (convert) का मतलब है तारीख को टेक्स्ट फॉर्मेट में परिवर्तन करना। प्रोग्रामर्स यह कार्य आमतौर पर करते हैं क्योंकि यह डेटा को सहेजने, प्रदर्शित करने या किसी API में भेजने के लिए सुविधाजनक होता है।

## कैसे करें?: (How to:)
Gleam में तिथि को स्ट्रिंग में बदलने के लिए, नीचे कोड का उदाहरण दिया गया है:

```gleam
import gleam/io
import gleam/calendar.{Date}
import gleam/erlang/time

pub fn main() {
  let today = time.now() // वर्तमान तिथि और समय प्राप्त करें
  let date_string = date.to_iso8601(today) // ISO 8601 फॉर्मेट में बदलें
  io.println(date_string) // स्ट्रिंग प्रदर्शित करें
}
```

आउटपुट:

```
"2023-03-15T12:49:36Z"
```

यह कोड वर्तमान दिनांक और समय को ISO 8601 स्टैंडर्ड स्ट्रिंग में बदलेगा और उसे प्रिंट करेगा।

## गहराई से जानकारी (Deep Dive)
तिथि को स्ट्रिंग में बदलने का अभ्यास कई सालों से हो रहा है क्योंकि यह डेटा को पढ़ने में आसान और मशीनों के बीच साझा करने योग्य बनाता है। ISO 8601 फॉर्मेट एक अंतरराष्ट्रीय मानक है जो विभिन्न सिस्टम्स और अनेक देशों में स्वीकार्य है। वैकल्पिक रूपों में Unix Timestamp या कस्टम फॉर्मेट शामिल हो सकते हैं, लेकिन ये यूजर-फ्रेंडली नहीं होते। Gleam के 'calendar' मॉड्यूल में 'Date' टाइप उपलब्ध है जिसे 'to_iso8601' फंक्शन का उपयोग करके स्ट्रिंग में बदला जा सकता है। जबकि 'erlang' मॉड्यूल का 'time' फंक्शन हमें मौजूदा समय प्रदान करता है।

## संबंधित स्रोत (See Also)
- [ISO 8601 Date and Time format](https://en.wikipedia.org/wiki/ISO_8601)
