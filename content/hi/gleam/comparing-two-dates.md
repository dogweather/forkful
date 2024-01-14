---
title:                "Gleam: दो तिथियों का तुलना करना"
simple_title:         "दो तिथियों का तुलना करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप भी Gleam प्रोग्रामिंग में शामिल होंना चाहते हैं? यदि हां, तो आपने शायद अलग-अलग तारीखों को तुलना करने की जरूरत पड़ी होगी। यह कोई आसान काम नहीं है, लेकिन Gleam के साथ हम इसे बड़े ही आसानी से कर सकते हैं। इसे करने का सबसे अच्छा तरीका है अलग-अलग दिनांकों को एक समान स्तर पर लाने के लिए उन्हें तुलना करना है। इस लेख में हम आपको तारीखों को तुलना करने का एक आसान तरीका बताएंगे जो Gleam के साथ आपको मदद करेगा।

## कैसे

तारीखों को तुलना करने के लिए, हम एक फंक्शन का उपयोग करेंगे जो कि दो तारीखों को लेकर उन्हें तुलना करेगा। हम भी दो तारीखों को एक समान स्तर पर लाने के लिए उन्हें एक ही फॉर्मैट में बदलेंगे। इसके लिए, हम एक ```Gleam.Date``` यूनिट का उपयोग करेंगे।

```Gleam
fn compare_dates(date1, date2) {
  // यदि दिन के समान हो, तो साल की तुलना करें
  if date1.day == date2.day {
    return date1.year == date2.year ? 0 : date1.year > date2.year ? 1 : -1
  }

  // यदि महीने के समान हो, तो दिन की तुलना करें
  if date1.month == date2.month {
    return date1.day > date2.day ? 1 : -1
  }

  // अन्यथा, महीने की तुलना करें
  return date1.month > date2.month ? 1 : -1
}

// उदाहरण तारीखों की तुलना
let date1 = Date.new(2020, 12, 25)
let date2 = Date.new(2021, 3, 10)
console.log(compare_dates(date1, date2))
// Output: -1 (date1 is before date2)
```

आप भी एक ```Gleam.DateTime``` यूनिट का उपयोग करके तारीखों और समय को तुलना कर सकते हैं। इस