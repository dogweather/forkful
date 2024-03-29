---
date: 2024-01-20 17:34:22.054049-07:00
description: "\u0930\u0938\u094D\u091F \u092E\u0947\u0902 \u0926\u094B \u0924\u093E\
  \u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0915\
  \u0930\u0928\u093E \u092F\u093E\u0928\u0940 \u0926\u094B DateTime \u0938\u0902\u0938\
  \u094D\u0925\u093E\u090F\u0901 \u0915\u0947 \u092C\u0940\u091A \u0938\u092E\u092F\
  \u093E\u0928\u094D\u0924\u0930 \u0915\u094B \u0938\u092E\u091D\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0910\
  \u0938\u093E \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0947 \u0905\u0928\
  \u0941\u0915\u094D\u0930\u092E \u0915\u094B \u091C\u093E\u0901\u091A\u0928\u0947\
  , \u0938\u092E\u092F \u0938\u0940\u092E\u093E\u090F\u0901 \u0924\u092F \u0915\u0930\
  \u0928\u0947,\u2026"
lastmod: '2024-03-13T22:44:51.983646-06:00'
model: gpt-4-1106-preview
summary: "\u0930\u0938\u094D\u091F \u092E\u0947\u0902 \u0926\u094B \u0924\u093E\u0930\
  \u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\
  \u0928\u093E \u092F\u093E\u0928\u0940 \u0926\u094B DateTime \u0938\u0902\u0938\u094D\
  \u0925\u093E\u090F\u0901 \u0915\u0947 \u092C\u0940\u091A \u0938\u092E\u092F\u093E\
  \u0928\u094D\u0924\u0930 \u0915\u094B \u0938\u092E\u091D\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0910\u0938\u093E\
  \ \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0947 \u0905\u0928\u0941\u0915\
  \u094D\u0930\u092E \u0915\u094B \u091C\u093E\u0901\u091A\u0928\u0947, \u0938\u092E\
  \u092F \u0938\u0940\u092E\u093E\u090F\u0901 \u0924\u092F \u0915\u0930\u0928\u0947\
  ,\u2026"
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
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
