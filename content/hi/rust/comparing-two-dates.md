---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:34:22.054049-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रस्ट में दो तारीखों की तुलना करना यानी दो DateTime संस्थाएँ के बीच समयान्तर को समझना। प्रोग्रामर्स ऐसा तारीखों के अनुक्रम को जाँचने, समय सीमाएँ तय करने, और समयाधारित आंकड़ों को संसाधित करने के लिए करते हैं।

## How to: (कैसे करें:)
```Rust
use chrono::{DateTime, Utc};

fn main() {
    let start_date: DateTime<Utc> = Utc.ymd(2023, 3, 14).and_hms(4, 30, 0);
    let end_date: DateTime<Utc> = Utc.ymd(2023, 3, 15).and_hms(17, 45, 0);
    
    if start_date < end_date {
        println!("शुरुआत की तारीख end_date से पहले है।");
    } else if start_date == end_date {
        println!("दोनों तारीखें एक समान हैं।");
    } else {
        println!("शुरुआत की तारीख end_date के बाद है।");
    }
}
```
आउटपुट:
```
शुरुआत की तारीख end_date से पहले है।
```

## Deep Dive (गहराई से जानकारी):
DateTime संरचना का उपयोग करके तारीखों की तुलना करना रस्ट में क्रोनो (chrono) लाइब्रेरी से संभव होता है। क्रोनो लाइब्रेरी, जो Rust में समय और तारीख से संबंधित कार्यों के लिए एक व्यापक समाधान प्रदान करती है, समय-क्षेत्र को संभालने के लिए `Utc` जैसे मॉड्यूल का उपयोग करती है। ऐतिहासिक संदर्भ में, Rust ने समय के संचालन को स्टैंडर्ड लाइब्रेरी में शामिल किया है, लेकिन अधिक विशेषताओं और सटीकता के लिए क्रोनो जैसी थर्ड-पार्टी लाइब्रेरीज आम तौर पर पसंद की जाती हैं। अल्टरनेटिवली, अन्य भाषाओं की तरह, Rust में भी तिथियों की तुलना के लिए पैकेज का चुनाव किया जा सकता है, जैसे कि time या date_time क्रेट्स। तुलना संचालन समय क्षेत्रों और लीप सेकंड्स जैसे जटिल मुद्दों को ध्यान में रखकर किया जाता है।

## See Also (इसे भी देखें):
- [Chrono Documentation](https://docs.rs/chrono/)
- [Rust Programming Language Official Website](https://www.rust-lang.org/)
- [The Rust Standard Library Documentation](https://doc.rust-lang.org/std/)
