---
title:                "Rust: भविष्य या भूतकाल में एक तिथि की गणना"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी सोचा है कि अगले या पिछले तारीख को कैसे निर्धारित किया जाए? यह एक बहुत ही उपयोगी कौशल है जो रशिया कोडिंग में जानना बहुत ही जरूरी है। इसलिए इस ब्लॉग पोस्ट में हम आपको बताएंगे कि हम अगले या पिछले तारीख को कैसे निर्धारित कर सकते हैं।

## कैसे करें

इस पोस्ट को पढ़ने के बाद हम आपको स्पष्ट और सरल ढंग से बताएंगे कि आप इस कौशल को कैसे अपने कोडिंग में इस्तेमाल कर सकते हैं। इसके लिए हम रशिया के कोडिंग के उदाहरण और संकेतों के साथ "```Rust ... ```" कोड ब्लॉक देंगे।

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    // Calculate date 7 days in the future
    let seven_days_from_now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
        + (7 * 24 * 60 * 60);
        
    // Convert to date format
    let date_in_future = UNIX_EPOCH
        .checked_add(std::time::Duration::from_secs(seven_days_from_now))
        .unwrap();
        
    // Print output in readable format
    println!("Date 7 days from now: {:?}", date_in_future);
    
    // Calculate date 30 days in the past
    let thirty_days_ago = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
        - (30 * 24 * 60 * 60);
        
    // Convert to date format
    let date_in_past = UNIX_EPOCH
        .checked_add(std::time::Duration::from_secs(thirty_days_ago))
        .unwrap();
        
    // Print output in readable format
    println!("Date 30 days ago: {:?}", date_in_past);
}
```

यहां हमने रशिया हेतु स्टैंडर्ड लाइब्रेरी का उपयोग किया है जो हमें सिस्टम के समय को प्राप्त करने और उसे डेटा टाइम के फॉर्मेट में बदलने में मदद करता है। हमने सरल उदाहरण दिया है जो आपको समझने में आसानी करेगा। आप भी अपने कोड में इस तरह का लोजिक इस्तेमाल कर सकते हैं।

## डीप डाइव

जब हम इस काम को