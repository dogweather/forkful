---
date: 2024-01-20 17:45:31.706949-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Rust \u092E\
  \u0947\u0902, \u0939\u092E `reqwest` \u0915\u094D\u0930\u0947\u091F \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0906\
  \u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\
  \u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902."
lastmod: '2024-03-13T22:44:51.959519-06:00'
model: gpt-4-1106-preview
summary: "Rust \u092E\u0947\u0902, \u0939\u092E `reqwest` \u0915\u094D\u0930\u0947\
  \u091F \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \u0915\u0947 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\u0947\u092C \u092A\
  \u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to (कैसे करें):
Rust में, हम `reqwest` क्रेट का इस्तेमाल करके आसानी से वेब पेज डाउनलोड कर सकते हैं:

```rust
use reqwest; // HTTP क्लाइंट लाइब्रेरी
use std::error::Error;

#[tokio::main] // एसिंक्रनस एन्ट्री प्वाइंट
async fn main() -> Result<(), Box<dyn Error>> {
    let url = "https://www.rust-lang.org"; // वेबसाइट का URL
    let resp = reqwest::get(url).await?; // URL से डेटा लेना 

    let body = resp.text().await?; // वेब पेज की सामग्री पढ़ना
    println!("Web page content:\n\n{}", body); // सामग्री प्रदर्शित करना

    Ok(())
}
```

जब आप इस प्रोग्राम को चलाएंगे, आपको Rust की ऑफिसियल वेबसाइट की HTML सामग्री दिखाई देगी।

## Deep Dive (गहराई में जानकारी):
वेब पेज डाउनलोड करना HTTP रिक्वेस्ट और रिस्पॉन्स के कॉन्सेप्ट्स पर आधारित होता है। `reqwest` Rust में मशहूर चुनाव है क्योंकि यह सहज और पॉवरफुल है। पिछले दिनों में, `hyper` जैसी लाइब्रेरी ज्यादा कम लेवल के ऑपरेशन्स के लिए इस्तेमाल होती थी। `reqwest` `hyper` पर आधारित है लेकिन यूजर के लिए ज्यादा सरल इंटरफेस प्रदान करता है।

वैकल्पिक तरीके में `curl` जैसे command-line उपकरण भी हैं जो स्क्रिप्ट में इस्तेमाल किए जा सकते हैं।

## See Also (और देखें):
- reqwest crate documentation: https://docs.rs/reqwest/
- Rust async book: https://rust-lang.github.io/async-book/
- HTTP client guidelines: https://www.arewewebyet.org/topics/http-clients/

इन लिंक्स में आपको Rust में HTTP रिक्वेस्ट्स से संबंधित और भी गहराई जानकारी मिलेगी।
