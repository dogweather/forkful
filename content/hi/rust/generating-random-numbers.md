---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

संगणक संख्याओं का उत्पन्न होना एक प्रादेभाषिक सुविधा है जिसका उपयोग करके प्रोग्रामर यादृच्छिक संख्याएँ प्राप्त कर सकते हैं। यह गेमिंग, आईओटी, अद्वितीय ID उत्पन्न करने, क्रिप्टोग्राफी आदि जैसे क्षेत्रों में अत्यंत महत्वपूर्ण है।

## कैसे (How To)

कोड उदाहरण और सैंपल आउटपुट Rust में  

```Rust
// Rust में रैंडम संख्या उत्पन्न करने के लिए:
extern crate rand;
use rand::Rng;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1, 101);
    println!("तेरी यादृच्छिक संख्या है: {}", secret_number);
}
```
जब उपयोगकर्ता इस कोड को चलाता है, तो वह हर बार 1 से 100 तक की एक यादृच्छिक संख्या प्राप्त करेगा।

## गहराई में (Deep Dive)

1. ऐतिहासिक प्रसंग: यादृच्छिक संख्या उत्पन्न करने का विचार संगणकों की खोज के साथ ही शुरू हुआ
2. विकल्प: Rust में , कुछ अन्य लाइब्रेरीज भी हैं जैसे कि "rand::random", "rand::RngCore" जिनका उपयोग यादृच्छिक संख्याओं को उत्पन्न करने के लिए किया जा सकता है।
3. क्रियान्वयन विवरण: "rand::thread_rng()" एक थ्रेड के लिए यादृच्छिकसंख्या उत्पादक लौटाता है, "gen_range()" फ़ंक्शन एक रेंज में यादृच्छिक संख्याएँ उत्पन्न करता है।

## आगे देखें (See Also)

अधिक जानकारी के लिए, निम्नलिखित स्रोतों की जांच करें:

1. [Rust के औपचारिक दस्तावेज़ीकरण](https://doc.rust-lang.org/rand/rand/index.html)
2. [The Rust Programming Language (संगणक प्रोग्रामिंग भाषा)](https://doc.rust-lang.org/book/)