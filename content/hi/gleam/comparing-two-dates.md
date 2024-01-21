---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:34.680278-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना करने का मतलब है दो तारीखों के बीच के अंतराल या काल क्रम को समझना। प्रोग्रामर इसे घटनाओं को शेड्यूल करने, डेडलाइंस की जांच करने और समय पर आधारित लॉजिक लागू करने के लिए करते हैं।

## How to: (कैसे करें:)
```gleam
import gleam/calendar.{Date, time_since}

fn compare_dates(date1: Date, date2: Date) -> i64 {
  time_since(date1, date2).in_seconds
}

// उदाहरण:
fn main() {
  let date1 = Date(year: 2023, month: 1, day: 15)
  let date2 = Date(year: 2023, month: 2, day: 14)

  let difference = compare_dates(date1, date2)
  difference
  // परिणाम: -30 * 24 * 60 * 60 सेकंड 
}
```

## Deep Dive (गहराई से जानकारी)
ग्लीम एक आधुनिक प्रोग्रामिंग भाषा है जो टाइप सेफ़्टी और एर्गोनॉमिक्स पर जोर देती है। पहले के जमाने में, तारीख की तुलना करने के लिए मैन्युअल कैलकुलेशन्स की जरूरत होती थी, पर Gleam के `calendar` मॉड्यूल में `Date` और `time_since` जैसे फंक्शन हमें यह काम आसानी से करने की सजगता देते हैं। अन्य भाषाओं में भी ऐसे फंक्शन हैं, पर Gleam की सख्त टाइप सिस्टम के कारण यह और भी सटीक हो जाता है। तुलना आमतौर पर सेकंड्स में की जाती है, और तारीखें मानक ISO फॉर्मेट में होनी चाहिए।

## See Also (और भी देखें)
- Understanding Time Zones in Programming: Related concepts when dealing with dates across different time zones.
- [Gleam's GitHub repository](https://github.com/gleam-lang/gleam): ग्लीम का सोर्स कोड और लेटेस्ट अपडेट।