---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:32:28.026318-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना इसके पार्सिंग, मैनिपुलेशन, और XML दस्तावेज़ों के निर्माण को शामिल करता है, जिनका उपयोग उनके संरचित और व्यापक प्रारूप के कारण डेटा एक्सचेंज के लिए किया जाता है। प्रोग्रामर्स अनगिनत सिस्टम्स के साथ इंटरफ़ेस के लिए XML को हैंडल करते हैं जहाँ XML डेटा की सामान्य भाषा है।

## कैसे करें:
Gleam मूल रूप से XML का समर्थन नहीं करता है, इसलिए हम `gleam_xml` जैसी एक बाहरी लाइब्रेरी का उपयोग करेंगे। पहले, इसे अपनी `gleam.toml` में जोड़ें:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

अब, XML को पार्स और निर्मित करें:

```rust
import gleam/xml

// XML पार्स करें
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// XML निर्मित करें
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

`xml.render(node)` के लिए नमूना आउटपुट है:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## गहराई से अध्ययन
XML का अर्थ है eXtensible Markup Language, जो W3C की एक विनिर्देश है और HTML की बहन है। यह 90 के दशक के अंत से है। Gleam के लिए, XML के साथ काम करना कुछ हद तक समय में पीछे जाने जैसा लगता है। JSON और Protocol Buffers अधिक ट्रेंडी हैं, लेकिन XML का पुराने सिस्टम और कुछ उद्योगों में व्यापक उपयोग होना इसे अभी भी प्रासंगिक बनाए रखता है।

Erlang इकोसिस्टम में `xmerl` जैसे विकल्प मौजूद हैं; हालांकि, `gleam_xml` लाइब्रेरी Gleam उपयोगकर्ताओं के लिए एक अधिक नैतिक दृष्टिकोण प्रदान करती है। यह मौजूदा Erlang लाइब्रेरीज पर आधारित है लेकिन एक Gleam-अनुकूल API को उजागर करती है। XML के प्रति Gleam का दृष्टिकोण सादगी और सुरक्षा की ओर लक्ष्यित है, बॉयलरप्लेट को कम करता है और प्रकार सुरक्षा पर जोर देता है।

कार्यान्वयन की दृष्टि से, `gleam_xml` सहित XML लाइब्रेरीज आमतौर पर DOM-समान संरचनाएँ प्रदान करती हैं। इसमें नोड्स, विशेषताएँ, और नेस्टेड एलिमेंट्स शामिल हैं, जो संभावित रूप से बड़े और जटिल दस्तावेज़ों को संभालने के लिए Erlang के पैटर्न मिलान और समानांतरता मॉडल का लाभ उठाती हैं।

## देखने के लिए
- Hex पर `gleam_xml` लाइब्रेरी: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- W3C द्वारा आधिकारिक XML मानक: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- व्यापक XML ट्यूटोरियल: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- XML प्रोसेसिंग के लिए Erlang की `xmerl` दस्तावेज़ीकरण: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
