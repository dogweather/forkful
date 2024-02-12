---
title:                "स्ट्रिंग इंटरपोलेशन"
aliases: - /hi/rust/interpolating-a-string.md
date:                  2024-01-20T17:52:11.424167-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
जब हम किसी टेक्स्ट में वेरिएबल्स के वैल्यूज को शामिल करते हैं, उसे स्ट्रिंग इंटरपोलेशन कहते हैं। यह प्रोग्रामर्स के लिए महत्वपूर्ण है क्योंकि इससे डायनामिक वैल्यूज को स्ट्रिंग में आसानी से जोड़ना संभव हो जाता है।

## कैसे करें? (How to:)
```Rust
fn main() {
    let name = "अमित";
    let age = 25;
    // सिंपल स्ट्रिंग इंटरपोलेशन उदाहरण
    println!("नाम: {}, उम्र: {}", name, age);
}
```
आउटपुट:
```
नाम: अमित, उम्र: 25
```

## गहराई से जानकारी (Deep Dive)
स्ट्रिंग इंटरपोलेशन की अवधारणा पुरानी है और अनेक प्रोग्रामिंग भाषाओं में व्यवहार में लायी गयी है। Rust में यह `format!` मैक्रो के जरिये कार्य करता है। 

विकल्प के रूप में हम `format!` फंक्शन का उपयोग करके भी स्ट्रिंग को इंटरपोलेट कर सकते हैं, जो कि एक नया `String` रिटर्न करता है।

```Rust
fn main() {
    let name = "अमित";
    let age = 25;
    let greeting = format!("नमस्ते, मेरा नाम {} है और मेरी उम्र {} साल है।", name, age);
    println!("{}", greeting);
}
```

स्ट्रिंग इंटरपोलेशन को इम्प्लीमेंट करने के लिए Rust मैक्रोज एक टेम्प्लेट में वैल्यूज को संजोती हैं और फिर कंपाइल टाइम पर स्ट्रिंग बनाती हैं, इसलिए यह तेज़ और इफ़ेक्टिव होता है।

## और भी देखें (See Also)
- Rust documentation for `format!` macro: [https://doc.rust-lang.org/std/macro.format.html](https://doc.rust-lang.org/std/macro.format.html)
- Stack Overflow discussion on string interpolation in Rust: [https://stackoverflow.com/questions/tagged/string-interpolation+rust](https://stackoverflow.com/questions/tagged/string-interpolation+rust)
- Rust By Example on formatted print: [https://doc.rust-lang.org/rust-by-example/hello/print.html](https://doc.rust-lang.org/rust-by-example/hello/print.html)
