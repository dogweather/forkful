---
title:                "Rust: वर्तमान तारीख प्राप्त करना"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# क्यों?

क्या आप एक प्रोग्रामर हैं और रस्ट प्रोग्रामिंग को सीखने को तैयार हैं? यदि हाँ, तो आपको जानना जरूरी है कि रस्ट में वर्तमान दिनांक कैसे मिला सकता है। आपको अपने कोड में वर्तमान दिनांक का उपयोग करके दर्शाना होता है कि आपके प्रोग्राम में फाइल विभिन्नताओं को आप ने कब बनाया या अंतरित किया। अतः वर्तमान दिनांक की प्राप्ति सीखना आपके लिए फायदेमंद हो सकता है।

## कैसे करें?

यदि आपको रस्ट अनुप्रयोग बनाने का अनुभव है, तो आप समझते होंगे कि वर्तमान दिनांक कैसे प्राप्त करें। लेकिन अगर आप एक नए यूजर हैं या रस्ट के साथ काम करना शुरू कर रहे हैं, तो आपको इसके बारे में अधिक जानने की जरूरत हो सकती है। इसलिए, मैं आपको स्टेप बाय स्टेप बताऊंगा कि कैसे रस्ट में वर्तमान दिनांक को ले सकते हैं:

```rust
use std::time::SystemTime;

fn main() {
    // Get current date and time
    let current_date = SystemTime::now();

    // Convert to UNIX time (seconds since Jan 1, 1970)
    let unix_time = current_date
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("Time went backwards");

    println!("Current date: {}", unix_time.as_secs());
}
```

उपरोक्त कोड को रन करने पर, आपको वर्तमान दिनांक की स्टम्प (UNIX टाइम) मिलेगी। कुछ इस तरह:

```bash
Current date: 1603031413
```

### रस्ट का समय प्रबंधन

आप आश्चर्य होंगे कि रस्ट के लिए समय प्रबंधन कितने आसान है। यह `std::time` मॉड्यूल के साथ उपलब्ध बहुत कुछ के साथ आता है जो आपको बाकी प्रोग्रामिं