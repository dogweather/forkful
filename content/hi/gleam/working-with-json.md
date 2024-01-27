---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON का मतलब होता है JavaScript Object Notation, जो डेटा को स्टोर और ट्रांसमिट करने का एक स्टैंडर्ड फॉर्मेट है। प्रोग्रामर्स JSON का इस्तेमाल इसलिए करते हैं क्योंकि यह ह्यूमन और मशीन-रीडेबल दोनों है और अलग-अलग प्रोग्रामिंग भाषाओं के बीच डेटा शेयर करने में आसान है।

## How to: (कैसे करें:)
Gleam में JSON के साथ काम करने के लिए, आपको `gleam/serde` लाइब्रेरी की जरूरत होगी। आइए कुछ सिंपल कोड उदाहरण देखें:

```gleam
import gleam/serde
import gleam/serde/json

pub struct Person {
  name: String,
  age: Int,
}

// JSON एनकोडिंग
pub fn encode_person(person: Person) -> String {
  let encoded = json.encode(person)
  case encoded {
    Ok(json) -> json
    Error(_) -> "Error in encoding".to_string()
  }
}

// JSON डिकोडिंग
pub fn decode_person(json_string: String) -> Result(Person, String) {
  json.decode(json_string)
}

// सैंपल आउटपुट
fn main() {
  let person = Person(name: "सोनम", age: 30)
  let encoded_person = encode_person(person)
  io.println(encoded_person) // '{"name":"सोनम","age":30}'

  let decoded_person = decode_person(encoded_person)
  io.println(decoded_person) // Ok(Person(name: "सोनम", age: 30))
}
```

## Deep Dive (गहराई से जानकारी)
JSON का इस्तेमाल 2000s की शुरुआत से हो रहा है जब डग क्रोकफोर्ड ने इसे सबके लिए पोपुलर बनाया। XML और YAML जैसे अल्टरनेटिव्स भी हैं, लेकिन JSON का सिंपल सिंटैक्स और रीडेबिलिटी ने इसे पसंदीदा बना दिया। Gleam में, `gleam/serde` लाइब्रेरी JSON को सीरियलाइज़ और डिसीरियलाइज़ करने की एक आसान और टाइप-सेफ तरीका प्रोवाइड करती है।

## See Also (और जानकारी के लिए)
- Official Gleam Documentation: [Gleam Language](https://gleam.run)
- Introduction to JSON: [JSON.org](https://www.json.org/json-en.html)
- Comparison of JSON and XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
