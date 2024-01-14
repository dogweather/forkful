---
title:                "Elm: दो तिथियों की तुलना"
simple_title:         "दो तिथियों की तुलना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो तारीखों को तुलना करने का यही एक बहुत अच्छा तरीका है कि हम दो आईएसओ 8601 स्ट्रिंग्स को एक साथ मिलाकर उन्हें मोडर्न क्रियाकलापों में प्रस्तुत करें।

## कैसे

```Elm
import Date exposing (fromIsoString, toIsoString, Day)

-- Y-m-d format
let date1 = "2021-06-10"
let date2 = "2021-06-15"

-- Converting strings to dates
let convertedDate1 = fromIsoString date1 -- returns date in Elm's Date type
let convertedDate2 = fromIsoString date2

-- Finding difference between two dates
let diff = Day.diff convertedDate1 convertedDate2

-- Printing output
toIsoString diff -- returns "5d"

```

## गहराई में जाएं

दो तारीखों को तुलना करने में कुछ और कठिनाई भी हो सकती है।इस उदाहरण में, हम एक अलग प्रकार के स्ट्रिंग हो रहा है जो हमारे date1 और जून 10 को रहता है और पहले स्तर तारीखों के बेहद सटीक तो है जिसमें अतिरिक्त दिनों को भी शामिल करता है। इसलिए, इसके अतिरिक्त, हम एक date का विद्यमान दिन और तारीख को जोड़ सकते हैं जो हमें एक सटीक तारीख मिलता है।

```Elm
import Date exposing (fromCalendarDate, toIsoString)

-- M-d-y format
let date1 = "06-10-2021"

-- Converting string to date
let convertedDate1 = fromCalendarDate date1 -- returns date in Elm's Date type

-- Adding a day to the date
let resultDate = Date.add Day convertedDate1 1

-- Printing output
toIsoString resultDate -- returns "2021-06-11"
```

## देखें भी

- [Elm डेटा और समय](https://guide.elm-lang.org/dates_and_times/)
- [ISO 8601 स्ट्रिंग](https://en.wikipedia.org/wiki/ISO_8601)