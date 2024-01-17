---
title:                "यादृच्छिक संख्याएं उत्पन्न करना"
html_title:           "Rust: यादृच्छिक संख्याएं उत्पन्न करना"
simple_title:         "यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Random numbers उसे संख्याओं के समूह के लिए कहा जाता है जो किसी भी प्रकार की क्रमबद्धता के बिना उत्पन्न होते हैं। यह प्रोग्रामर्स द्वारा उसे उनकी कोड की variability को बढ़ाने के लिए किया जाता है।

## कैसे करें:
```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let num: i32 = rng.gen_range(0..10);
    println!("Random number is: {}", num);
}

```
Output:
```
Random number is: 5
```
## गहराई में जाएं:
Random number generator एक algorithm होता है जो यादृच्छिक संख्याओं को generate करता है। इसे विभिन्न applications में इस्तेमाल किया जाता है जैसे cryptography और simulations में। रस्ट के साथ आप विभिन्न random number libraries का इस्तेमाल कर सकते हैं जैसे rand और rand_core। यह उदाहरण में हमने thread_rng() उपयोग किया है, जो एक नया random number generator प्रस्तुत करता है, और gen_range() से हम दिए गए range के बीच कोई एक उपयुक्त रैंडम संख्या अनुकूलित करते हैं। 

## और भी देखें:
- [The Rust Book chapter on generating random numbers](https://doc.rust-lang.org/book/ch07-04-randomness.html)
- [The rand crate documentation](https://docs.rs/rand/0.8.3/rand)
- [The rand_core crate documentation](https://docs.rs/rand_core/0.6.2/rand_core)