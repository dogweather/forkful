---
title:                "Rust: दो तिथियों की तुलना करना"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

आप कभी कभी दो तारीखों को तुलना करने की आवश्यकता महसूस कर सकते हैं, चाहे आपको दो आर्ग्यूमेंट्स में से किसी को चुनना हो या दो घड़ियों के बीच कितना समय अंतर पता करना हो। इस लेख में, हम आपको दो तारीखों की तुलना करने के लिए रस्ट प्रोग्रामिंग के माध्यम से आसान तरीके बताएंगे। 

## कैसे करें 

```rust
// दो तारीखों को मिलाने के लिए `chrono` के उपयोग का उदाहरण

use chrono::{NaiveDate, Datelike};

fn compare_dates(date1: NaiveDate, date2: NaiveDate) {
    if date1 > date2 {
        println!("{} इसके बाद आता है {}", date1, date2);
    } else if date2 > date1 {
        println!("{} इसके बाद आता है {}", date2, date1);
    } else {
        println!("दोनों तारीखें बराबर हैं");
    }
}

fn main() {
    let date1 = NaiveDate::from_ymd(2021, 3, 15);
    let date2 = NaiveDate::from_ymd(2021, 3, 25);
    
    compare_dates(date1, date2);
}

// Output:
// 2021-03-25 इसके बाद आता है 2021-03-15
``` 

कॉड केस्टडी में, हम `chrono` लाइब्रेरी का उपयोग करके दो तारीखों को `NaiveDate` ऑब्जेक्ट में रूपांतरित करते हैं और उन्हें तुलना करने के लिए `>`, `<`, और `==` ऑपरेटर्स का उपयोग करते हैं। उपरोक्त कोड का आउटपुट इसी तरह होगा जैसा कि हमने अपेक्षित था। 

## गहराई में जाएं

तारीखों के समय पर काम करने में थोड़ी मुश्किल हो सकती है, इसलिए आपको उनके बीच का अंतर_प्रतीक्षा_से उलझन दूर करने के लिए `diff()` फ़ंक्शन का भी उपयोग कर सकते हैं। इसका उपयोग करके, आप दोनों तारीखों के बीच अ