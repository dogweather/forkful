---
title:                "json के साथ काम करना"
html_title:           "Ruby: json के साथ काम करना"
simple_title:         "json के साथ काम करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
JSON (JavaScript Object Notation) एक विशेष तरीके से डेटा को स्टोर और ट्रांसफर करने के लिए प्रोग्रामर्स द्वारा उपयोग किया जाता है। प्रोग्रामर्स JSON का उपयोग डेटा को स्टोर करने के लिए, डेटा को अन्य सर्वर्स पर भेजने के लिए या डेटा फिल्टर करने के लिए करते हैं। 

## कैसे करें:
Ruby में JSON से काम करने के लिए, हम जब जावास्क्रिप्ट कोड का उपयोग करते हैं तब अपने कोड में ```json``` सेंटेंस इनपुट पहले और तीसरा डेटा को स्टोर करने के लिए करते हैं।

```Ruby
require 'json'
# JSON से डेटा पहले और तीसरा डेटा को स्टोर करें
data = '{"name":"John", "age":30, "city":"New York"}'
parsed_data = JSON.parse(data)
# पहले डेटा का आउटपुट
puts "Name: #{parsed_data['name']}"
# द्वितीय डेटा का आउटपुट
puts "Age: #{parsed_data['age']}"
# तीसरा डेटा का आउटपुट
puts "City: #{parsed_data['city']}"
```

आउटपुट:
```
Name: John
Age: 30
City: New York
```

## गहराई में जाएँ:
निरिंतर समय के उस समय से जब सबसे पहले JSON डेटा फॉर्मेट विकसित किया गया था (1999), JSON बहुत अधिक पसंद किया जाता है क्योंकि यह बहुत सरल है, हलका है, और डेटा पारगमन में कोई भी चूक के लिए संभावना बहुत कम है। अन्य विकल्पों में शामिल हैं XML, CSV, और YAML। जबकि JSON का रूपांतरण IL Serialization या थिर्ड पार्टी लाइब्रेरी का उपयोग करके किया जा सकता है।

## और भी देखें:
- [JSON.org](https://www.json.org/json-en.html)
- [Official Ruby JSON Documentation](https://ruby-doc.org/stdlib-2.5.0/libdoc/json/rdoc/JSON.html)