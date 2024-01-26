---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:36:06.832405-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों को स्ट्रिंग से पार्स करने का मतलब है टेक्स्ट में दी गई तारीख को पढ़ना और उसे ऐसे फॉर्मेट में बदलना जिसे प्रोग्राम समझ सकें। प्रोग्रामर इसका इस्तेमाल उपयोगकर्ता इनपुट, डेटा स्टोरेज, या एपीआई इंटरैक्शन में करते हैं।

## How to: (कैसे करेंः)
Elixir में DateTime स्ट्रक्चर का उपयोग करके आप आसानी से स्ट्रिंग से तारीख पार्स कर सकते हैं। नीचे एक उदाहरण है कि कैसे यह किया जा सकता है:

```elixir
# datetime.ex

defmodule DateTimeParser do
  def parse_date_string(date_string) do
    case DateTime.from_iso8601(date_string) do
      {:ok, datetime, _} -> datetime
      {:error, _} -> {:error, "Invalid date format"}
    end
  end
end

# उपयोग करने का उदाहरण:

{datetime, _} = DateTimeParser.parse_date_string("2023-03-25T13:45:30Z")
IO.inspect datetime
```

परिणाम:
```elixir
# यदि सही है तो
%DateTime{
  year: 2023,
  month: 3,
  day: 25,
  hour: 13,
  minute: 45,
  second: 30,
  time_zone: "Etc/UTC",
  zone_abbr: "Z",
  utc_offset: 0,
  std_offset: 0,
  ...
}

# यदि गलत है तो
{:error, "Invalid date format"}
```

## Deep Dive (गहराई से जानकारी)
Elixir में तारीखों को पार्स करने की क्षमता, इसे ऑवर 2014 में प्रकाशित होने वाले `DateTime` मॉड्यूल के साथ लाई गई थी। ISO 8601 स्टैंडर्ड सबसे आम है और इसका इस्तेमाल अंतरराष्ट्रीय प्रारूप में तारीख और समय को प्रस्तुत करने के लिए होता है। Elixir `DateTime.from_iso8601/1` फ़ंक्शन आपको इस प्रारूप की स्ट्रिंग को आसानी से पार्स करने की सुविधा देता है।

अगर आपको अलग फॉर्मेट्स की तारीखें पार्स करनी हैं, तो आप `Timex` जैसी लाइब्रेरीज का सहारा ले सकते हैं, जो अधिक फ्लेक्सिबिलिटी और फ़ीचर्स प्रदान करती हैं।

## See Also (देखें भी)
- [DateTime Module Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601 Standard](https://en.wikipedia.org/wiki/ISO_8601)
- [Timex Library on Hex.pm](https://hex.pm/packages/timex)
