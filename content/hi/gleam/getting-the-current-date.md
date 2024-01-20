---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:14:49.324837-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
वर्तमान तारीख प्राप्त करना यह है कि आप प्रोग्राम में आज की तारीख कैसे ले सकते हैं। कार्यक्रमकर्ता लॉगिंग, समयसीमा की जांच और उपयोगकर्ता इंटरफेस में तारीख दिखाने के लिए इसका उपयोग करते हैं।

## कैसे करें: (How to:)
```gleam
import gleam/calendar
import gleam/io

// वर्तमान तारीख प्राप्त करें
fn main() {
  let today = calendar.local_now()
  io.print(today)
}
```

उत्पादित होने वाला प्रिंट आउटपुट:
```
{calendar.Date(year: 2023, month: 4, day: 1)}
```

## गहराई से जानकारी (Deep Dive)
वर्तमान तारीख को प्राप्त करने के लिए `gleam/calendar` मॉड्यूल Gleam में एक नया अतिरिक्त है। पहले, डेवलपर्स को बाहरी लाइब्रेरी जैसे कि `calendar` के साथ काम करना पड़ता था। इरलांग और ईलिक्सिर जैसे विकल्पों के साथ ग्लीम के संयोजन से इस कार्यक्षमता को सरल और अधिक कुशल बनाया गया है। इसे कार्यान्वित करते समय समय क्षेत्रों और स्थानीयकरण का भारी ध्यान रखा जाता है।

## यह भी देखें (See Also)
- Gleam's official documentation on the `calendar` module: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Working with time zones in Gleam: https://hexdocs.pm/gleam_stdlib/gleam/time/
- Erlang's calendar documentation for background understanding: http://erlang.org/doc/man/calendar.html