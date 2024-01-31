---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:38:34.457898-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग से पार्स करना मतलब है स्ट्रिंग रूप में दी गई तारीख को प्रोग्राम में उपयोग करने योग्य डेटा फॉर्मेट में बदलना। इसे प्रोग्रामर्स इसलिए करते हैं क्योंकि तारीखें अक्सर डेटाबेस, लॉग फाइलों या यूजर इनपुट में स्ट्रिंग फॉर्म में मिलती हैं, और इन्हें संसाधित करने के लिए हमें इन्हें पार्स करना होता है।

## How to: (कैसे करें:)

Rust में तारीख को स्ट्रिंग से पार्स करना आसान है। आइये `chrono` क्रेट का इस्तेमाल करके देखते हैं:

```Rust
use chrono::{NaiveDate, ParseError};

fn parse_date_from_string(date_str: &str) -> Result<NaiveDate, ParseError> {
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
}

fn main() {
    let date_str = "2023-03-14";
    match parse_date_from_string(date_str) {
        Ok(date) => println!("Parsed date is {}", date),
        Err(e) => println!("Error parsing date: {}", e),
    }
}
```

अगर स्ट्रिंग सही फॉर्मेट में है तो आउटपुट होगा:

```
Parsed date is 2023-03-14
```

और अगर स्ट्रिंग गलत फॉर्मेट में है:

```
Error parsing date: input contains invalid characters
```

## Deep Dive (गहराई में जानकारी)

तारीख को पार्स करना कोई नई जरूरत नहीं है। पर जैसे-जैसे कंप्यूटिंग का विकास हुआ, डेटा स्वरूपों और लाइब्रेरीज के डिज़ाइन में सुधार होता गया। Rust में, `chrono` क्रेट पार्सिंग के लिए मानक हो गई है। इससे पहले, तारीखों को हाथ से पार्स करने की जरूरत होती थी, जिससे गलतियां और असुरक्षा की आशंका बढ़ जाती थी।

`chrono` मोड्यूल `NaiveDate`, `NaiveTime`, `NaiveDateTime`, `DateTime`, और अन्य कक्षाएं प्रदान करता है जिनके उपयोग से समय और तारीख को सही तरीके से पार्स और हैंडल किया जा सकता है। 

विकल्पों की बात करें तो, अगर हम `chrono` क्रेट का इस्तेमाल नहीं करना चाहते हैं, तो हम Rust के `time` क्रेट का उपयोग कर सकते हैं या स्ट्रिंग मेन्युअली पार्स कर सकते हैं।
