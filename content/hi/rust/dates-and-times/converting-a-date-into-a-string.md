---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases:
- /hi/rust/converting-a-date-into-a-string/
date:                  2024-01-20T17:38:12.064338-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख को स्ट्रिंग में बदलने का मतलब है कि आप दिनांक वाले डेटा को पाठ रूप में परिवर्तित कर रहे हैं। कार्यक्रमकर्ता इसे इसलिए करते हैं क्योंकि यह तारीखों को अधिक पठनीय और समझने में आसान बनाता है। 

## कैसे करें:

Rust में तारीख को स्ट्रिंग में बदलने के लिए, हम `chrono` क्रेट का उपयोग कर सकते हैं:

```Rust
// `chrono` क्रेट जोड़ें
extern crate chrono;
use chrono::{DateTime, Utc, Local};

fn main() {
    // UTC दिनांक और समय
    let now_utc: DateTime<Utc> = Utc::now();
    println!("{}", now_utc.format("%Y-%m-%d %H:%M:%S"));

    // स्थानीय दिनांक और समय
    let now_local: DateTime<Local> = Local::now();
    println!("{}", now_local.format("%Y-%m-%d %H:%M:%S"));
}
```

आउटपुट इस प्रकार हो सकता है:
```
2023-03-28 14:10:05
2023-03-28 19:40:05
```

## गहन ज्ञान

ऐतिहासिक रूप से, तारीख और समय को स्ट्रिंग में बदलने का काम C और C++ जैसी पुरानी भाषाओं में `strftime` फ़ंक्शन का इस्तेमाल करके किया जाता था। Rust में, `chrono` क्रेट यह सुविधा प्रदान करती है जो अधिक सुरक्षित है और उपयोग में आसानी के साथ-साथ त्रुटि-मुक्त कोडिंग की सुविधा देती है। `format` मैक्रो का उपयोग करके हम आसानी से दिनांक को किसी भी इच्छित स्वरूप में बदल सकते हैं।

`chrono` क्रेट `DateTime` तत्वों का उपयोग करता है, जिसे `Utc` या `Local` जैसे टाइम ज़ोन स्पेसिफिक ट्रेट्स के साथ जोड़कर यूनिवर्सल या स्थानीय समय प्राप्त किया जा सकता है।

विकल्पों की बात करें तो, Rust में `time` क्रेट भी होती है जिसका उपयोग कर सकते हैं, परंतु `chrono` अधिक लोकप्रिय और व्यापक है।

## यह भी देखें:

1. Chrono Documentation: [https://docs.rs/chrono/](https://docs.rs/chrono/)
3. strftime फ़ॉर्मैट स्पेसिफिकेशन: [https://pubs.opengroup.org/onlinepubs/007908799/xsh/strftime.html](https://pubs.opengroup.org/onlinepubs/007908799/xsh/strftime.html)
