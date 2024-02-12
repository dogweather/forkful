---
title:                "HTML विश्लेषण"
aliases:
- /hi/rust/parsing-html.md
date:                  2024-02-03T19:13:31.317775-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

रस्ट में HTML पार्स करने का मतलब है HTML दस्तावेज़ों से डेटा निकालना, जो कि वेब स्क्रेपिंग, डेटा एक्सट्रैक्शन या वेब क्रॉलर्स बनाने के लिए आवश्यक है। प्रोग्रामर्स ऐसा वेब से जानकारी का संग्रहण स्वचालित करने, वेब सामग्री का विश्लेषण करने, या एक मंच से दूसरे मंच पर सामग्री माइग्रेट करने के लिए करते हैं।

## कैसे करें:

रस्ट में HTML पार्स करने के लिए, आप अक्सर `scraper` क्रेट का उपयोग करेंगे, जो HTML दस्तावेज़ों को ट्रैवर्स और मैनिपुलेट करने के लिए एक उच्च-स्तरीय इंटरफेस प्रदान करता है।

सबसे पहले, अपनी `Cargo.toml` में `scraper` जोड़ें:

```toml
[dependencies]
scraper = "0.12.0"
```

अगला, ये एक साधारण उदाहरण है जो एक दिए गए HTML स्ट्रिंग से सभी लिंक URL निकालता है:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">लिंक 1</a>
        <a href="http://example.com/2">लिंक 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("मिला लिंक: {}", link);
    }
}
```

आउटपुट:

```
मिला लिंक: http://example.com/1
मिला लिंक: http://example.com/2
```

इस उदाहरण में, हम एक साधारण HTML दस्तावेज को पार्स करते हैं ताकि सभी `<a>` तत्वों को खोज सकें और उनके `href` गुणों को निकाल सकें, प्रभावी रूप से दस्तावेज़ में सभी लिंकों के URL प्रिंट करते हैं। `scraper` लाइब्रेरी HTML पार्सिंग को सरल बनाती है और CSS सेलेक्टर्स का उपयोग करके विशेष तत्वों को चुनने में मदद करती है, जिससे यह रस्ट में वेब स्क्रेपिंग कार्यों के लिए एक प्रमुख साधन बन जाता है।
