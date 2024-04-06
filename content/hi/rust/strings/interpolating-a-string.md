---
date: 2024-01-20 17:52:11.424167-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0906\u0909\
  \u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.943572-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

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
