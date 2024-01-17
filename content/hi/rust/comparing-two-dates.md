---
title:                "दो तारीखों की तुलना करना"
html_title:           "Rust: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

"## यह क्या है और क्यों?

दो तिथियों को तुलना करना एक ऐसी प्रक्रिया है जिसके माध्यम से आप दो अलग तिथियों को एक दूसरे से तुलना कर सकते हैं। प्रोग्रामर्स इसको करते हैं ताकि वे दो तारीखों के बीच अंतर को पता कर सकें और अपने कोड में इसका उपयोग कर सकें।

## कैसे:

इसके लिए, हम `DateTime` क्लास और `chrono` पैकेज का उपयोग करेंगे। नीचे दिए गए कोड ब्लॉक में आप इसका उपयोग देख सकते हैं:

```rust
use chrono::{DateTime, Datelike, Timelike, Utc};

// दो तारीखों को तुलना करना
let date1 = DateTime::parse_from_str("2022-01-01 00:00:00", "%Y-%m-%d %H:%M:%S").unwrap();
let date2 = DateTime::parse_from_str("2030-12-31 23:59:59", "%Y-%m-%d %H:%M:%S").unwrap();
// तुलना करें
if date1 > date2 {
    println!("{} बड़ा तारीख है", date1);
} else {
    println!("{} बड़ा तारीख है", date2);
}

// यूटीसी का उपयोग करने से वर्तमान समय का पता लगाएं
let now = Utc::now();
println!("वर्तमान समय {}", now);
```

आउटपुट:

```
2030-12-31 23:59:59 UTC बड़ा तारीख है
वर्तमान समय 2021-08-25 00:00:00 UTC
```

## डीप डाइव:

दो तिथियों को तुलना करने की इतिहास बहुत पुराना है। पहले, मानव योग्य समय के लिए कैलेंडर बनाने के लिए संख्याओं को प्रयोग किया गया। लेकिन आजकल यह प्रत्येक प्रोग्रामिंग भाषा में सहजता से उपलब्ध है।

इसके अलावा, एक्सेंट्रिकल और बगीश समयों को तुलना करने के लिए अन्य विकल्प भी उपलब्ध हैं। इनमें से एक है, संख्या तमाम। इसमें, दो तारीखों के मध्य अंतर की गणना में समस्या हो सकती है जिससे गलत परिणाम दिया जा सकता है।

टेक्निकल डिटेल्स के लिए, यह पैकेज `chrono` माध्यम से तुलना का काम करने के लिए स्पष्ट फ़ंक्शन और अलग तरीकों का उपयोग करता है। इसके लिए, यह `DateTime` क्लास के अंदर दो तारीखों के मध्य अंतर को मापता है।

## देखें भी:

- [Rust Language Guide](https://doc.rust-lang.org/book/)
- [Official Rust Date and Time API Docs](https://docs.rs/chrono/0.4.19/chrono/)
- [Comparing Dates in Other Programming Languages](https://www.techbeamers.com/compare-date-times-rust/)