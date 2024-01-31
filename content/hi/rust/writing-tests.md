---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेस्ट लिखना यानी आपके कोड का परीक्षा करना; इससे बग्स को पहचानकर सॉफ्टवेयर की गुणवत्ता सुधारी जाती है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह कोड की दुरुस्ती और फ्यूचर अपग्रेडेशन में मददगार होता है। 

## जानिए कैसे:
```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn another() {
        // ये टेस्ट फेल हो जाएगा, उदाहरण के लिए
        assert_eq!(1 + 1, 3);
    }
}
```
आउटपुट होगा:
```
test tests::it_works ... ok
test tests::another ... FAILED

failures:

---- tests::another stdout ----
note: test failed, expected: `assert_eq!(1 + 1, 3)`, with values:
             left: `2`,
            right: `3`
```

## गहराई से समझ:
रस्ट में टेस्ट्स बनाने का तरीका जावा, सी++ जैसी पुरानी भाषाओं से थोड़ा हटकर है। रस्ट की `#[cfg(test)]` और `#[test]` एन्नोटेशन्स टेस्ट्स को आसान बनाती हैं और `cargo test` कमांड उन्हें चलाती है। हालांकि, अन्य लैंग्वेज में JUnit या pytest जैसे फ्रेमवर्क्स का इस्तेमाल करते हैं। रस्ट में `assert` मैक्रो इस्तेमाल करके हम सही आउटपुट की जांच करते हैं।

## यह भी देखें:
- [The Rust Programming Language's Testing chapter](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example's section on testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Rust Documentation for `assert` macros](https://doc.rust-lang.org/std/macro.assert.html)
