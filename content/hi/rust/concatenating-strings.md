---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:36:24.453542-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग्स को जोड़ने का मतलब है कई स्ट्रिंग्स को एक साथ चेपना। प्रोग्रामर इसे डेटा को आसानी से संगठित करने और यूजर इंटरफेस में मैसेज दिखाने के लिए करते हैं।

## How to: (कैसे करें)

Rust में स्ट्रिंग्स को जोड़ने के कुछ तरीके:

```Rust
fn main() {
    // तरीका 1: format! मैक्रो का उपयोग करें
    let first_name = "रोहित";
    let last_name = "कुमार";
    let full_name = format!("{} {}", first_name, last_name);
    println!("पूरा नाम: {}", full_name);
    
    // तरीका 2: '+' ऑपरेटर का उपयोग करें
    let string1 = "नमस्ते ".to_string();
    let string2 = "दुनिया!";
    let greeting = string1 + string2; // Note: string1 is now moved and can't be used anymore
    println!("अभिवादन: {}", greeting);
    
    // तरीका 3: push_str() मेथड का उपयोग करें
    let mut message = "हैलो".to_string();
    message.push_str(" रस्ट!");
    println!("सन्देश: {}", message);
}

```

सैंपल आउटपुट्स:
```
पूरा नाम: रोहित कुमार
अभिवादन: नमस्ते दुनिया!
सन्देश: हैलो रस्ट!
```

## Deep Dive (गहरी जानकारी)

स्ट्रिंग्स को जोड़ना बहुत पहले से प्रोग्रामिंग का हिस्सा है। Rust में, स्ट्रिंग्स के दो मुख्य प्रकार हैं: `str` जो कि स्टेटिक स्ट्रिंग स्लाइस है और `String` जो कि हीप पर सामग्री के साथ एक ग्रोइंग स्ट्रिंग है। `String` के साथ काम करते समय, यह जरूरी है कि हम मेमोरी एलोकेशन पर ध्यान दें; Rust सुरक्षित मेमोरी मैनेजमेंट के लिए जाना जाता है। जब हम '+' ऑपरेटर का उपयोग करते हैं, तो पहला स्ट्रिंग हमेशा `String` होना चाहिए और इसके बाद केवल `&str` स्ट्रिंग स्लाइस हो सकते हैं। यह ऑपरेशन ओनरशिप को लेता है, जिसका मतलब है कि पहले स्ट्रिंग का स्वामित्व बदल जाता है। कई कॉन्कैटिनेशन के लिए `format!` मैक्रो का उपयोग करना बेहतर होता है क्योंकि यह अधिक फ्लेक्सिबल होता है और ओनरशिप इश्यूज़ को रोकता है।

## See Also (और देखो)

- Rust के आधिकारिक डॉक्यूमेंटेशन में स्ट्रिंग्स पर मार्गदर्शिका: [Rust Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust by Example पर स्ट्रिंग्स पर उदाहरण: [Rust by Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- Rust प्रोग्रामिंग भाषा के बारे में अधिक जानने के लिए 'The Rust Programming Language' बुक: [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- std::string मॉड्यूल के डॉक्यूमेंटेशन: [std::string](https://doc.rust-lang.org/std/string/index.html)