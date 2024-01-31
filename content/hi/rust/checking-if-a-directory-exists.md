---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:59:01.048718-07:00
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व जांचने का मतलब है कि हम पता करते हैं कि कोई फोल्डर सिस्टम में मौजूद है या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे त्रुटियों को रोकना और फाइल ऑपरेशन्स को अधिक सुरक्षित बनाना संभव होता है।

## How to: (कैसे करें:)
Rust में डायरेक्टरी के अस्तित्व की जाँच के लिए `std::path::Path` और `std::fs` स्टैंडर्ड लाइब्रेरी के फंक्शन का उपयोग करें।

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/some/directory/path");

    if path.exists() {
        println!("डायरेक्टरी मौजूद है!");
    } else {
        println!("डायरेक्टरी मौजूद नहीं है।");
    }
}
```

जब आप इस कोड को चलाएंगे, तो यह प्रिंट करेगा:

- अगर `/some/directory/path` मौजूद है: `डायरेक्टरी मौजूद है!`
- नहीं तो: `डायरेक्टरी मौजूद नहीं है।`

## Deep Dive (गहराई में जानकारी):
`Path` और `PathBuf` Rust के दो प्राथमिक प्रकार हैं जो फाइल सिस्टम पथ को मैनेज करते हैं। `Path::exists` फंक्शन `Metadata` प्राप्त करने की कोशिश करता है, जो उपस्थिति की पुष्टि करने के लिए है। यह विधि न केवल डायरेक्टरी के लिए, बल्कि फाइल्स के लिए भी काम करती है।

विकल्प के तौर पर, आप `std::fs::metadata` या `std::fs::read_dir` का भी इस्तेमाल कर सकते हैं। मगर, `Path::exists` का इस्तेमाल अधिक आम और सीधा है।

एक बात का ध्यान रखें कि रेस कंडीशन्स के कारण, जांच और ऑपरेशन करने के बीच की स्थितियां बदल भी सकती हैं। इसलिए, हमेशा एरर हैंडलिंग करना अच्छा रहता है।

## See Also (और भी जानकारी):
- Rust by Example के [File I/O section](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [`std::path::Path` documentation](https://doc.rust-lang.org/std/path/struct.Path.html)
- [`std::fs` module documentation](https://doc.rust-lang.org/std/fs/)
