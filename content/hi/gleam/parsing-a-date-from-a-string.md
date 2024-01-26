---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:17.679826-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को एक स्ट्रिंग से पार्स करना मतलब है उसे पढ़ पाने योग्य और प्रोग्राम करने योग्य फॉर्मेट में बदलना। प्रोग्रामर इसलिए ऐसा करते हैं क्योंकि यूजर इनपुट या डेटा स्टोरेज के समय स्ट्रिंग्स में तारीखें आमतौर पर सेव होती हैं और इन्हें संभालना, मैनिप्युलेट करना, और विश्लेषण के लिए सही फॉर्मेट में पार्स करना पड़ता है।

## How to: (कैसे करें:)
```gleam
import gleam/expect
import gleam/date_time.{StringToDateParser, Date}

pub fn demo() {
  // उदाहरण के तौर पर "2023-01-15" वाली एक स्ट्रिंग
  let date_string = "2023-01-15"
  let parser = StringToDateParser.default()
  
  // स्ट्रिंग को पार्स करना
  let date_result = StringToDateParser.parse(parser, date_string)
  
  case date_result {
    Ok(date) -> 
      date
    Error(e) -> 
      panic(e)
  }
}
```

सैंपल आउटपुट:
```gleam
Date(year: 2023, month: 1, day: 15)
```

## Deep Dive (गहराई से जानकारी)
पहले, तारीखों को पार्स करने के लिए प्रोग्रामर रेगुलर एक्सप्रेशन और मैनुअल पार्सिंग का उपयोग करते थे, जो थकाऊ और एरर-प्रोन हो सकता था। आजकल, लाइब्रेरीज़ जैसे कि `gleam/date_time` इस प्रोसेस को सरल बनाती हैं। ग्लीम में, `StringToDateParser.default()` एक डिफ़ॉल्ट डेट पार्सर प्रदान करता है जो ISO 8601 स्टैंडर्ड का समर्थन करता है। `StringToDateParser.parse` फंक्शन इस पार्सर का इस्तेमाल करके स्ट्रिंग को `Date` स्ट्रक्चर में कन्वर्ट करता है। यह काम करते समय, यह तारीखों की वैधता भी चेक करता है, जैसे कि फरवरी में 30 तारीख होना अवैध है।

## See Also (और जानकारी के लिए)
- ISO 8601 डेट और टाइम फॉर्मेट स्टैंडर्ड: [ISO 8601 Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)

ध्यान रहे, ये लिंक्स अंग्रेज़ी में हैं क्योंकि हिंदी में इसकी अधिक जानकारी न होने का सम्भावना है। Gleam और उसकी पार्सिंग टेकनीक्स पर और जानने के लिए इंटरनेट में सर्च कर सकते हैं।
