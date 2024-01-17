---
title:                "HTML को पार्स करना।"
html_title:           "Rust: HTML को पार्स करना।"
simple_title:         "HTML को पार्स करना।"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, कंप्यूटर प्रोग्रामिंग में एक अहम् और उपयोगी क्रिया है जो वेब पेजों की संरचना और तथ्यों को विश्लेषण करने में सहायक होती है। पार्सिंग हेतु उपयोग किए जाने वाले प्रोग्राम HTML को डिजाइन की गयी संरचना से अलग करके उपयोगकर्ता को भली भांति स्पष्ट सूचना देते हैं। कंप्यूटर प्रोग्रामर्स इसका उपयोग वेब स्क्रैपिंग, डेटा माइनिंग, और सेमांटिक एनालिसिस में करते हैं।

## कैसे करें:

यदि आप HTML पार्सिंग करना चाहते हैं, तो आप रस्ट भाषा में बहुत ही सरल और प्रभावी ढंग से कर सकते हैं। आप निम्नलिखित कोड ब्लॉक में दिए गए उदाहरण को अपने भाषा में अनुकूलित करके इनका उपयोग कर सकते हैं। यह कोड आपको एक HTML पेज के संरचना को डिजाइन की गई तरीके से अलग करके के उपयोगकर्ता को स्पष्ट सूचना देगा।

    ```Rust
    // Importing necessary libraries
    use std::fs;
    use scraper::{Html, Selector};

    // Loading HTML page into a string
    let html = fs::read_to_string("index.html").expect("Invalid HTML file");

    // Creating a selector to target specific elements
    let selector = Selector::parse("div.container h1").unwrap();

    // Creating an HTML tree structure to parse the elements
    let document = Html::parse_document(&html);

    // Looping through all elements matching the selector
    for element in document.select(&selector) {
        // Gets the inner HTML content of the selected element
        let text = element.inner_html();
        println!("{}", text);
    }
    ```

इसका आउटपुट निम्नलिखित हो सकता है:

    ```Rust
    Hello, World!
    ```

## गहराई में डूबो:

HTML पार्सिंग का इतिहास 1993 में सार्वजनिक रूप से शुरू हुआ था, जब टिम बर्नर्स ने वेब खोज इंजन के लिए HTML उपयोग करने का सुझाव दिया। पुरातन समय में, सिर्फ़ पाठ सबसे प्रमुख तथा सामान्‍य मुद्रा से दिखाई देती थी। साथ ही साथ, HTML पार्सिंग विकास के साथ-साथ सबसे सफल और उपयोगी काम साबित हुआ है। आप HTML पार्सिंग के अन्य विकल्पों की सभी जानकारी यहाँ देख सकते हैं: [https://crates.io/keywords/html-parsing](https://crates.io/keywords/html-parsing)

HTML पार्सिंग के अन्य विकल्पों की तुलना में रस्ट अपनी अत्यधिक कार्यसमर्थता और टाइप सुरक्षा के कारण उपयुक्त है। इसके अलावा, रस्ट के बारे में और जानकारी के लिए संबंधित स्रोतों के लिंक नीचे दिए गए हैं।

## और भी देखें:

- [https://www.rust-lang.org/](https://www.rust-lang.org/)
- [https://crates.io/](https://crates.io/)
- [https://doc.rust-lang.org/book/](https://doc.rust