---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:32:05.926069-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

किसी तिथि को भविष्य या भूत में गणना करने का मतलब है कि एक निश्चित तिथि से कितने दिन पहले या बाद की तिथि का पता लगाना। प्रोग्रामर इसका उपयोग अनुसूचियां बनाने, समय-सीमाएँ तय करने और अवधि के हिसाब से तिथियों की गणना करने के लिए करते हैं।

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
