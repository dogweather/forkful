---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक data format है जो data आदान-प्रदान के लिए उपयोगी है। Ruby में programmers JSON का इस्तेमाल इसलिए करते हैं क्योंकि यह human-readable है और web applications के साथ easily integrate हो जाता है।

## How to: (कैसे करें:)
Ruby में JSON के साथ काम करने के लिए आपको `json` library का इस्तेमाल करना पड़ता है। नीचे कुछ examples दिए गए हैं:

```Ruby
require 'json'

# JSON String to Ruby Hash
json_string = '{"name": "Amit", "age": 30, "city": "Delhi"}'
ruby_hash = JSON.parse(json_string)
puts ruby_hash['name']  # Output: Amit

# Ruby Hash to JSON String
ruby_hash = { name: "Priya", age: 25, city: "Mumbai" }
json_string = ruby_hash.to_json
puts json_string  # Output: {"name":"Priya","age":25,"city":"Mumbai"}
```

## Deep Dive (गहराई में जानकारी):
JSON, जो कि 2001 में Douglas Crockford द्वारा popularize किया गया, अब web का standard data interchange format बन गया है। इसे XML, YAML आदि की तुलना में अधिक पसंद किया जाता है क्योंकि यह more compact और तेज़ होता है। Ruby में `json` library core library का हिस्सा है, और native data types के बीच conversion को सरल बनाता है।

## See Also (और भी देखें):
- Ruby's official JSON library documentation: [ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- JSON specification and more information: [json.org](https://json.org)
- Ruby gem for parsing JSON: [github.com/flori/json](https://github.com/flori/json)
- Other serialization formats like YAML in Ruby: [ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html)
