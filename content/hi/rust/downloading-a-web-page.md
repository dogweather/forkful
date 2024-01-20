---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# वेब पेज डाउनलोड करना Rust में 

## क्या और क्यों?
वेब पेज डाउनलोड 'करना' का अर्थ है उसकी सामग्री को इंटरनेट से प्राप्त करना। प्रोग्रामर्स इसे वेब स्क्रेपिंग, एपीआई इंटरफेसिंग आदि के लिए करते हैं।

## कैसे करें:
```Rust
आवश्यकताओं को शामिल करें;
use reqwest;
use tokio;

async fn main() -> Result<(), reqwest::Error> {
    let res = reqwest::get("https://www.example.com").await?;
    
    println!("Status: {}", res.status());
    let body = res.text().await?;
    println!("Body:\n\n{}", body);
    
    Ok(())
}
```
इस उदाहरण में, हम डिफॉल्ट वेब ब्राउज़र का उपयोग कर रहे हैं और example.com पर एक GET अनुरोध भेज रहे हैं। हम `reqwest::get` फ़ंक्शन से रिस्पॉन्स प्राप्त करते हैं, फिर बॉडी और स्थिति को प्रिंट करते हैं।

## गहरा अध्ययन
"वेब पेज डाउनलोड" का इतिहास इंटरनेट के साथ ही शुरू हुआ। विभिन्न प्रोकोल, जैसे कि FTP, इसे संभव बनाते हैं। Rust में, हम विभिन्न क्रेट्स जैसे कि `reqwest` और `hyper` का उपयोग करके वेब डाउनलोड कर सकते हैं। 'reqwest' क्रेट का उपयोग करके उदाहरण दिया गया है क्योंकि यह `hyper` की तुलना में स्वचालित और सरल है।

## देखें भी
1. [Reqwest दस्तावेज़ीकरण](https://docs.rs/reqwest/)
2. [Rust आधिकारिक दस्तावेज़](https://www.rust-lang.org/learn)
3. [Rust प्रोग्रामिंग भाषा द्वारा हाउर्स ऑफ कोड](https://www.hackerrank.com/domains/tutorials/rust)