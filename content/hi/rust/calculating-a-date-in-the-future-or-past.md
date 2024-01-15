---
title:                "भविष्य या भूत की तारीख की गणना"
html_title:           "Rust: भविष्य या भूत की तारीख की गणना"
simple_title:         "भविष्य या भूत की तारीख की गणना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

डेटा की तारीख की गणना से कोई छूट नहीं की जा सकती है। इसलिए, इसका उपयोग शायद आपके दैनिक कार्यों में समय का निर्धारण करने में सहायक हो सकता है।

## कैसे करें

```rust
use chrono::{Utc, Local, Date, Datelike, NaiveDate};
fn calculate_date(year: i32, month: u32, day: u32) -> Date<Local> {
    let current_date = Local::today();
    let future_date = NaiveDate::from_ymd(year, month, day);
    let days_diff = future_date.num_days_from_ce() - current_date.num_days_from_ce();
    current_date + chrono::Duration::days(days_diff)
}

fn main() {
    let future_date = calculate_date(2021, 12, 25);
    let output = future_date.format("%A, %B %e, %Y").to_string();
    println!("{}", output);
}
```

उपरोक्त उदाहरण में, हम दिए गए तारीख की गणना करने के लिए `calculate_date` फ़ंक्शन का उपयोग करते हैं। यह हमें एक तारीख देता है जो आज से निर्धारित दिनों के बाद होती है। हम इस नए की प्रिंट किया हुआ प्रारूप को प्राप्त करने के लिए `format` फ़ंक्शन का भी उपयोग करते हैं। आप `'format'` की जगह पर अपनी पसंदीदा प्रारूप का उपयोग कर सकते हैं।

## गहराई में जाएं

डेटा की तारीख की गणना करने के लिए, हम `chrono` लाइब्रेरी का उपयोग कर सकते हैं। इसमें दो मुख्य सेटिंग्स हैं - समय क्षेत्र और अवधि शैली। हम `Local` को स्थानीय समय क्षेत्र के लिए और `Utc` को संयुक्त राष्ट्र के समय क्षेत्र के लिए उपयोग करते हैं। इन समय क्षेत्रों के साथ, आप अपने वर्तमान या भविष्य की तारीख को प्राप्त कर सकते हैं। हम `format` फ़ंक्शन के साथ तस्वीर तारीख का प्रारूप भी निर्दिष्ट कर सकते हैं, जो आपको ए