---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) सरलता से डेटा आदान-प्रदान के लिए एक प्रारूप है। इसका इस्तेमाल programmers इसलिए करते हैं क्योंकि यह हल्का, आसानी से पढ़ने योग्य, और भाषा-निरपेक्ष है। 

## How to: (कैसे करें:)
```Javascript
// JSON ऑब्जेक्ट को स्ट्रिंग में बदलना
let object = { name: "Amit", age: 30, city: "Delhi" };
let jsonString = JSON.stringify(object);
console.log(jsonString); // {"name":"Amit","age":30,"city":"Delhi"}

// स्ट्रिंग को JSON ऑब्जेक्ट में बदलना
let jsonString = '{"name":"Amit","age":30,"city":"Delhi"}';
let object = JSON.parse(jsonString);
console.log(object); // {name: "Amit", age: 30, city: "Delhi"}
```

## Deep Dive (गहराई से जानकारी)
JSON, 2001 में Douglas Crockford द्वारा विकसित, एक text-based data format है जो JavaScript की object literal notation से प्रेरित है। वैकल्पिक रूपों में XML और YAML हैं, लेकिन JSON की सरलता और Read/Write की गति इसे अधिक लोकप्रिय बनाती है। खासकर वेब APIs में इसका प्रयोग डेटा interchange format के रूप में होता है। JSON को जावास्क्रिप्ट के अलावा अन्य भाषाओं में भी इस्तेमाल करने के लिए libraries उपलब्ध हैं।

## See Also (और भी देखें)
- [JSON सरकारी वेबसाइट](https://www.json.org/json-en.html)
- [Mozilla Developer Network पर JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [W3Schools JSON Tutorial](https://www.w3schools.com/js/js_json_intro.asp)
