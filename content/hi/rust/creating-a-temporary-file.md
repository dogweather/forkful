---
title:    "Rust: एक अस्थायी फाइल बनाना"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## लेकर
अगर आप अपने ऑनलाइन प्रोजेक्ट में कुछ सामान्य फाइल्स को स्थायी रूप से स्थापित करना नहीं चाहते हैं तो सामान्यत: आप अस्थायी फ़ाइलों की जगह बनाने के लिए तुरंत फ़ाइलें बनाकर किसी भी संदर्भ में उन्हें उपयोग कर सकते हैं।

## कैसे करें
```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // अस्थायी फ़ाइल बनाएं
    let mut file = File::create("/path/to/file.txt")
        .expect("Unable to create file");

    // फ़ाइल में डेटा लिखें
    file.write_all(b"नमस्ते, दुनिया!")
        .expect("Unable to write data to file");

    // फ़ाइल बंद करें
    file.close()
        .expect("Unable to close file");
}
```

यहां, हमने `std::fs` और `std::io` का उपयोग करके रस्ता कोड के माध्यम से तो अस्थायी फ़ाइल बनाना सीखा। आप उन्हें आपके शर्टिकल में उपयोग कर सकते हैं या फिर अपने प्रोजेक्ट में भी।

## गहराई में
अस्थायी फ़ाइलों की बकाया क्रियाओं में अस्थायी फ़ाइलों का उपयोग करके हम वास्तव में सुचना संग्रहकों को बहुत सहायता प्रदान कर सकते हैं। इसके लिए, हमें फ़ाइलों को `File` आदि उपकरणों के पहले बंद करना होगा।

अगर आप कुछ और अधिक माहिती चाहते हैं, तो आप [रस्त आधिकारिक वेबसाइट](https://www.rust-lang.org/) पर जा सकते हैं और इससे संबंधित विभिन्न लेखों को पढ़ सकते हैं।

## देखें भी
- [Rust दस्तावेज़ीकरण](https://doc.rust-lang.org/1.39.0/book/foreword.html)
- [विभिन्न रस्त कार्यक्षमता कोड](https://doc.rust-lang.org/stable/rust-by-example/index.html)
- [R