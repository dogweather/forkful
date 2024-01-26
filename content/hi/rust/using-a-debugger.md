---
title:                "डीबगर का उपयोग करना"
date:                  2024-01-26T04:11:24.009774-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक डीबगर का उपयोग करना आपके कोड की निष्पादन को देखने के लिए अपने आप को एक्स-रे दृष्टि प्रदान करने जैसा है। प्रोग्रामर इसे बग्स का पता लगाने, प्रोग्राम के प्रवाह को समझने और सुनिश्चित करने के लिए करते हैं कि उनका कोड जितना संभव हो उतना साफ हो। यह ऐसे है जैसे आपके पास एक दोस्त हो जो सटीक रूप से बताए कि आप कहाँ गिर गए हैं।

## कैसे:

रस्ट विभिन्न प्रकार के डिबगर्स का समर्थन करता है, लेकिन एक सामान्य `gdb` GNU/Linux के लिए या macOS के लिए `lldb` है। आप `rust-gdb` या `rust-lldb` का भी उपयोग कर सकते हैं, जो रुस्ट मूल्यों को सुंदर प्रिंट करने के लिए रैपर्स हैं। यहाँ एक झलक है:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

इसे डीबग करने के लिए, डीबग जानकारी के साथ कंपाइल करें:

```shell
$ rustc -g counter.rs
```

फिर इसे `rust-gdb` में रन करें:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## गहन गोता

डीबगिंग का प्रचलन पंच कार्ड्स के *ये ओल्डे टाइम्स* से चला आ रहा है, और इसका विकास एक दैविक वरदान रहा है। रस्ट अपनी स्वयं की टूलिंग प्रदान करता है जिसे GDB और LLDB के साथ एकीकरण के कारण भाषा के सिस्टम-स्तर की प्रकृति के लिए उपयुक्त बनाया गया है।

रस्ट कोड को डीबग करने के वैकल्पिक तरीके में एकीकृत विकास वातावरण (IDEs) का उपयोग शामिल है जिनमें उनके निर्मित डिबगर्स होते हैं, जिन्हें कुछ लोग अधिक सहज पाते हैं। लोकप्रिय IDEs में CLion के साथ Rust प्लगइन या Visual Studio Code के साथ Rust एक्सटेंशन शामिल हैं।

कार्यान्वयन के लिए, रस्ट डीबग प्रतीक उत्पन्न करता है जिन्हें ये डीबगर समझते हैं, जो कोड के माध्यम से कदम रखने, ब्रेकपॉइंट सेट करने और चरों का निरीक्षण करते समय आपकी समझ को खोए बिना महत्वपूर्ण है।

## यह भी देखें

- डीबगिंग पर रस्ट बुक: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- गलतियों और डीबगिंग पर रस्ट बाय एक्जाम्पल का दृष्टिकोण: https://doc.rust-lang.org/rust-by-example/error.html
- रस्ट लैंग्वेज सर्वर (RLS) जो VS Code के रस्ट एक्सटेंशन को सक्षम बनाता है: https://github.com/rust-lang/rls
- विजुअल स्टूडियो कोड के साथ रस्ट का डीबगिंग: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust