---
title:    "Rust: नियमित अभिव्यक्तियों का उपयोग करना"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों 

Regular expressions उस सभी के लिए महत्वपूर्ण हैं जो programming में लगे होते हैं। ये उन्हें टेक्स्ट मैनिपुलेशन को आसान और तेज़ बनाते हैं। 

## कैसे

Regular expressions का syntax ```Rust
  Regex::new("pattern").unwrap();
```
होता है। ```new``` एक ```Regex``` struct बनाता है जो pattern को compile करता है। इसके अन्दर हम इन्हें दो symbols के साथ use करते हैं: ```^``` जो string का starting point दिखाता है और ```$``` जो string का ending point दिखाता है। ये pattern की समाप्ति का संकेत करते हैं। उदाहरण के लिए, अगर हमें किसी string में "rust" की presence check करनी है तो हम लिख सकते हैं: ```Rust
  Regex::new("^rust$").unwrap();
```

अगर ये pattern string में मिलता है तो हमें true मिलेगा। 

## गहराई में जाएं 

Regular expressions पर और गहराई से जानने के लिए, आप इन पढ़ सकते हैं: 

- [Rust के लिए Regex सीखे](https://docs.rs/regex/1.4.6/regex/) 
- [Regex कैसे काम करते हैं](https://docs.rs/regex/1.4.6/regex/#how-regexes-work)
- [Regex syntax का बदलाव](https://docs.rs/regex/1.4.6/regex/#syntax) 

## और भी पढ़ें 

- [Rust programming के लिए Regex सीखें](https://doc.rust-lang.org/book/ch09-06-tokio.html) 
- [Regex crate का अन्य use cases](https://crates.io/crates/regex#use-cases) 
- [Rust और regular expressions को साथ मिलाकर कैसे use किया जा सकता है](https://dev.to/emilstahl/rust-regex-processing-hyperlink-regular-expressions-38nl)