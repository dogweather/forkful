---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:36:39.229744-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML, जिसका पूरा नाम eXtensible Markup Language है, JSON का विस्तृत संस्करण जैसा है। जब आप पुराने सिस्टम्स, एंटरप्राइज सॉफ्टवेयर या उन APIs के साथ काम करते हैं जिन्होंने JSON की लहर को नहीं अपनाया, तो आपको XML के साथ जूझना पड़ेगा। जहां XML अपना महत्व रखता है वहां डेटा एक्सचेंज के लिए यह आवश्यक है।

## कैसे:
Rust में, आप `xml-rs` जैसे crates के साथ XML को संभाल सकते हैं। इंस्टाल करने के लिए अपने `Cargo.toml` में `xml-rs = "0.8"` जोड़ें। यहां एक सरल XML को पार्स करने का तरीका है:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("शुरुआत: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("टेक्स्ट: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("अंत: {}", name);
            }
            Err(e) => {
                println!("त्रुटि: {}", e);
            }
            _ => {}
        }
    }
}
```

आउटपुट:
```
शुरुआत: book
शुरुआत: title
टेक्स्ट: Rust in Action
अंत: title
शुरुआत: author
टेक्स्ट: Tim McNamara
अंत: author
शुरुआत: year
टेक्स्ट: 2021
अंत: year
अंत: book
```
यह कोड XML को स्ट्रीम-रीड करता है, शुरुआत और अंत तत्वों के साथ-साथ टेक्स्ट डेटा को संभालता है, प्रत्येक चरण का लॉग रखता है।

## गहराई में:
XML टेक वर्षों में एक वरिष्ठ है, जिसे 90 के दशक के अंत में वेब के लिए डिज़ाइन किया गया था। इसका डिज़ाइन पठनीयता (मशीनों और मनुष्यों दोनों के लिए) और व्यापक आत्म-वर्णनात्मक डेटा को बढ़ावा देता है।

विकल्प? ज़रूर, JSON मॉडर्न जा-टू है वेब APIs के लिए, हल्का और कम शोर। इस बीच, YAML कॉन्फ़िग्स के लिए इसके स्वच्छ लेआउट के साथ प्रशंसकों को आकर्षित कर रहा है। लेकिन XML इतनी जल्दी कहीं नहीं जा रहा है—इसके पीछे विशाल इंफ्रास्ट्रक्चर बने हुए हैं।

आंतरिक रूप से, Rust का XML पार्सिंग इटरेटर पैटर्न्स पर झुकाव रखता है, जो मेमोरी उपयोग को कम और प्रदर्शन को तेज रखता है। आपको JSON हैंडलिंग से परिचित लोगों के लिए एक वरदान के रूप में `serde-xml-rs` जैसे crates मिलेंगे।

## देखें भी:
Rust और XML पर अधिक के लिए:
- Rust के serde संगतता के लिए `serde-xml-rs`: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- आधिकारिक Rust डॉक्यूमेंटेशन (क्योंकि ब्रश अप करना कभी बुरा नहीं होता): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
