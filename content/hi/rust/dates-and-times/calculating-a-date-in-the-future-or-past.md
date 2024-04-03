---
date: 2024-01-20 17:32:05.926069-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Rust \u092E\u0947\
  \u0902 \u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0917\u0923\u0928\
  \u093E \u0915\u0947 \u0932\u093F\u090F `chrono` \u0915\u094D\u0930\u0947\u091F \u092C\
  \u0939\u0941\u0924 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0939\u0948\
  \u0964 \u0928\u0940\u091A\u0947 \u0915\u094B\u0921 \u0915\u0947 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:51.985306-06:00'
model: gpt-4-1106-preview
summary: "Rust \u092E\u0947\u0902 \u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\
  \u0940 \u0917\u0923\u0928\u093E \u0915\u0947 \u0932\u093F\u090F `chrono` \u0915\u094D\
  \u0930\u0947\u091F \u092C\u0939\u0941\u0924 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0939\u0948\u0964 \u0928\u0940\u091A\u0947 \u0915\u094B\u0921 \u0915\
  \u0947 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\
  \u0948\u0902."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

## कैसे करें:
Rust में तिथियों की गणना के लिए `chrono` क्रेट बहुत लोकप्रिय है। नीचे कोड के उदाहरण दिए गए हैं:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("अभी की तिथि और समय: {}", now);

    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("दो हफ्ते बाद की तिथि: {}", future_date);

    let past_date = now - two_weeks;
    println!("दो हफ्ते पहले की तिथि: {}", past_date);
}
```

संभावित आउटपुट:

```
अभी की तिथि और समय: 2023-04-10T12:34:56.789Z
दो हफ्ते बाद की तिथि: 2023-04-24T12:34:56.789Z
दो हफ्ते पहले की तिथि: 2023-03-27T12:34:56.789Z
```

## गहन जानकारी:
तिथियों की गणना प्राचीन समय से ही की जाती रही है, लेकिन प्रोग्रामिंग में इसकी सटीकता का महत्व विशेष रूप से है। `chrono` क्रेट Rust में तिथियों और समय के साथ काम करने का एक मानक तरीका है, हालांकि अन्य क्रेट्स जैसे कि `time` भी उपलब्ध हैं। `chrono` क्रेट न केवल समय की गणना कर सकती है, बल्कि timezone और फॉर्मेटिंग जैसे कार्य भी कर सकती है। गणना में प्रयोग की जाने वाली अवधियों को `Duration` के रूप में निर्दिष्ट किया जाता है और यह विभिन्न प्रकार की अवधियों को समर्थन करता है।

## संबंधित सोर्सेज:
- Rust `chrono` crate: [docs.rs/chrono](https://docs.rs/chrono/)
- Rust official website: [www.rust-lang.org](https://www.rust-lang.org/)
- Rust Date and Time concepts: [doc.rust-lang.org/book/ch10-02-traits.html](https://doc.rust-lang.org/book/ch10-02-traits.html)
- Rust `time` crate: [docs.rs/time](https://docs.rs/time/)
