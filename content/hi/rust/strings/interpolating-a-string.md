---
aliases:
- /hi/rust/interpolating-a-string/
date: 2024-01-20 17:52:11.424167-07:00
description: "\u091C\u092C \u0939\u092E \u0915\u093F\u0938\u0940 \u091F\u0947\u0915\
  \u094D\u0938\u094D\u091F \u092E\u0947\u0902 \u0935\u0947\u0930\u093F\u090F\u092C\
  \u0932\u094D\u0938 \u0915\u0947 \u0935\u0948\u0932\u094D\u092F\u0942\u091C \u0915\
  \u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  , \u0909\u0938\u0947 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\
  \u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u0915\u0939\u0924\u0947 \u0939\
  \u0948\u0902\u0964 \u092F\u0939 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u092E\u0939\u0924\u094D\
  \u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u0907\u0938\u0938\u0947\u2026"
lastmod: 2024-02-18 23:09:02.930897
model: gpt-4-1106-preview
summary: "\u091C\u092C \u0939\u092E \u0915\u093F\u0938\u0940 \u091F\u0947\u0915\u094D\
  \u0938\u094D\u091F \u092E\u0947\u0902 \u0935\u0947\u0930\u093F\u090F\u092C\u0932\
  \u094D\u0938 \u0915\u0947 \u0935\u0948\u0932\u094D\u092F\u0942\u091C \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u0909\
  \u0938\u0947 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\
  \u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u0915\u0939\u0924\u0947 \u0939\u0948\
  \u0902\u0964 \u092F\u0939 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u092E\u0939\u0924\u094D\u0935\
  \u092A\u0942\u0930\u094D\u0923 \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\
  \u093F \u0907\u0938\u0938\u0947\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
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
