---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:00.347242-07:00
description: "\u0915\u0948\u0938\u0947: #."
lastmod: '2024-03-13T22:44:51.978355-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे:


### रस्ट की स्टैंडर्ड लाइब्रेरी का उपयोग करते हुए (`chrono` क्रेट)
रस्ट स्टैंडर्ड लाइब्रेरी सीधे तारीख पार्सिंग को शामिल नहीं करती है, लेकिन व्यापक रूप से उपयोग की जाने वाली `chrono` क्रेट दिनांक और समय हेरफेर के लिए एक मजबूत समाधान है। पहले, अपनी `Cargo.toml` में `chrono` जोड़ें:

```toml
[dependencies]
chrono = "0.4"
```

फिर, एक दिनांक स्ट्रिंग को `NaiveDate` ऑब्जेक्ट में पार्स करने के लिए `chrono` का उपयोग करें:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Failed to parse date");

    println!("Parsed date: {}", date);
}

// नमूना आउटपुट:
// Parsed date: 2023-04-01
```

### रस्ट की उन्नत दिनांक-समय हैंडलिंग (`time` क्रेट)
अधिक उन्नत दिनांक-समय हैंडलिंग के लिए, जिसमें अधिक आरामदायक पार्सिंग शामिल है, `time` क्रेट पर विचार करें। पहले, इसे अपने `Cargo.toml` में शामिल करें:

```toml
[dependencies]
time = "0.3"
```

फिर, `Date` टाइप और `PrimitiveDateTime` का उपयोग करते हुए एक दिनांक स्ट्रिंग पार्स करें:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Failed to parse date and time");

    println!("Parsed datetime: {}", parsed_date);
}

// नमूना आउटपुट:
// Parsed datetime: 2023-04-01 12:34:56
```

दोनों उदाहरण दिखाते हैं कि कैसे रस्ट, तृतीय-पक्ष क्रेट्स की सहायता से, दिनांक स्ट्रिंग्स को निपटने योग्य दिनांक ऑब्जेक्ट्स में पार्स करना सुविधाजनक बनाता है, जिससे यह समयावधि डेटा से निपटने वाले सॉफ्टवेयर विकास के लिए एक शक्तिशाली उपकरण बन जाता है।
