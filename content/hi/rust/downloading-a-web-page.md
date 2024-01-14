---
title:                "Rust: वेब पृष्ठ डाउनलोड करना"
simple_title:         "वेब पृष्ठ डाउनलोड करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज को डाउनलोड करने का काम आसान नहीं है। एक वेब पेज को डाउनलोड करने के लिए आपको कई प्रक्रियाएं करनी पड़ती हैं। इसलिए, हमारे पास डाउनलोड करने के लिए एक शक्तिशाली और सुरक्षित तरीका होना चाहिए। रस्ट भाषा इससे उत्कृष्ट है क्योंकि यह अति सुरक्षित है, और विभिन्न प्लेटफार्मों पर उपयोगकर्ताओं को लक्षित करता है।

## कैसे करें 

आप रस्ट भाषामें एक वेब पेज को डाउनलोड कर सकते हैं आसानी से। सबसे पहले, आपको पूर्व-स्थापित कोड को सही ढंग से काम करने के लिए अपडेट करना होगा। तो चलिए देखते हैं कि वेब पेज को डाउनलोड करने के लिए कार्य में आप कैसे लगातार आगे बढ़ सकते हैं:

```Rust
extern crate reqwest;

use std::io::Write;

fn main() {
    let url = "https://www.example.com";
    let mut response = reqwest::get(url).expect("Unable to connect");
    let mut body = Vec::new();
    response.read_to_end(&mut body).expect("Unable to read response");
    let mut out = std::fs::File::create("example.html").expect("Unable to create file");
    out.write_all(&body).expect("Unable to write to file");
}
```

इस कोड में, हम पहले उपयोगी लाइब्रेरी "reqwest" और "Write" को प्रयोग करते हैं। उसके बाद, हम वेब पेज के URL को सेट करते हैं। फिर, हम रिस्पॉन्स ऑब्जेक्ट में वेब पेज को डाउनलोड करने के लिए एक GET रिक्वेस्ट भेजते हैं। आगे बढ़कर, हम रिस्पॉन्स ऑब्जेक्ट के भीतर से वेब पेज को पढ़ते हैं और एक फ़ाइल में लिखते हैं। अन्त में, हम सफलतापूर्वक इन कार्रवाईयों को पूरा करते हैं।

## गहर