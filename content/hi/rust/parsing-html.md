---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग का अर्थ होता है कि हम HTML डाक्युमेंट को अच्छी तरह से व्यवस्थित डाटा संरचना में बदल देते हैं। प्रोग्रामर्स इसे तभी करते हैं जब उन्हें वेबसाइट से डाटा एक्सट्रैक्ट करना होता है या HTML को मनिपुलेट करना होता है।

## कैसे:

Rust में HTMl पार्सिंग के लिए, हम सुनिश्चित कर सकते हैं कि हमारे पास html5ever लाइब्ररी है। यह उदाहरण दिखाता है कि कैसे Rust में HTML पार्सिंग होता है:

```Rust
use html5ever::{parse_document, serialize, tendril::TendrilSink};
use markup5ever_rcdom::RcDom;

let html_doc = "<!DOCTYPE html><html><head><title>Hello Rust!</title></head><body><h1>Hi there!</h1></body></html>";

let dom = parse_document(RcDom::default(), Default::default())
    .from_utf8()
    .read_from(&mut html_doc.as_bytes())
    .unwrap();

let mut bytes = vec![];
serialize(&mut bytes, &dom.document, Default::default()).unwrap();

let parsed_html = String::from_utf8(bytes).unwrap();

println!("{}", parsed_html);
```
उपरोक्त कोड की आउटपुट के रूप में, हमें HTML डॉक्यूमेंट मिलेगा:

```
<!DOCTYPE html><html><head><title>Hello Rust!</title></head><body><h1>Hi there!</h1></body></html>
```

## गहराई में: 

HTML पार्सिंग की आवश्यकता वेब स्क्रेपिंग जैसे कार्यों के लिए आती है, जो 90 के दशक में वेब की उभरती हुई प्रभावशालिता के साथ ही लोकप्रिय हुआ। Rust का उपयोग करके HTML पार्सिंग करना एक नया विचार है, जो स्पीड और मेमोरी सुरक्षा प्रदान करता है। 

HTML पार्सिंग के लिए विकल्पों में Python (BeautifulSoup), Javascript (Cheerio, JSDOM), और PHP DOM पार्सर शामिल हैं। 

HTML पार्सिंग का कार्य, एक HTML डाक्युमेंट को एक DOM (Document Object Model) में बदलने से होता है, जहां हर एक HTML टैग को एक नोड के रूप में प्रस्तुत किया जाता है।

## भी देखें:

1. [HTML5ever documentation](https://docs.rs/html5ever/0.25.1/html5ever/)
3. [Rust HTML parsing comparison](https://medium.com/@pmzubar/rust-and-the-curse-of-the-static-html-parser-564ce58f0d0e)