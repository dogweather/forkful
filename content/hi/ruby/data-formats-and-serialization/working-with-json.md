---
title:                "JSON के साथ काम करना"
aliases:
- /hi/ruby/working-with-json/
date:                  2024-02-03T19:24:57.481899-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) एक हल्का डेटा आदान-प्रदान प्रारूप है, जो ग्राहकों और सर्वरों के बीच डेटा विनिमय के लिए वेब अनुप्रयोगों में प्रचलित है। प्रोग्रामर्स बाह्य स्त्रोतों से प्राप्त डेटा को पार्स करने या API प्रतिक्रियाओं के लिए डेटा स्वरूपित करने के लिए, इसकी मानव-पठनीय संरचना का लाभ उठाते हुए, रूबी में JSON के साथ काम करते हैं।

## कैसे:

रूबी अपनी मानक लाइब्रेरी के साथ, JSON को पार्स करने और उत्पन्न करने के लिए सहज तरीके प्रदान करती है। इन कार्यों के लिए प्राथमिक मॉड्यूल `json` है, जिसे किसी भी रूबी अनुप्रयोग में आसानी से एकीकृत किया जा सकता है।

### JSON को पार्स करना:

एक JSON स्ट्रिंग को रूबी हैश में बदलने के लिए, आप `JSON.parse` मेथड का उपयोग कर सकते हैं।

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# आउटपुट: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSON उत्पन्न करना:

इसके विपरीत, एक रूबी हैश को JSON स्ट्रिंग में बदलने के लिए, आप `JSON.generate` मेथड या एक बार `json` लाइब्रेरी की आवश्यकता होने के बाद रूबी ऑब्जेक्ट्स पर उपलब्ध `to_json` मेथड का उपयोग करते हैं।

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# आउटपुट: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### तृतीय-पक्ष लाइब्रेरियां:

जबकि रूबी की मानक लाइब्रेरी मूल JSON हैंडलिंग को कवर करती है, कई परियोजनाएं संवर्धित कार्यक्षमता और प्रदर्शन के लिए तृतीय-पक्ष लाइब्रेरियों पर निर्भर करती हैं। एक लोकप्रिय विकल्प `Oj` (ऑप्टिमाइज़्ड JSON) है।

#### Oj के साथ पार्सिंग:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# आउटपुट: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Oj के साथ उत्पन्न करना:

Oj रूबी ऑब्जेक्ट्स से JSON उत्पन्न करने का एक तेज तरीका भी प्रस्तुत करता है:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# आउटपुट: {"name":"Samantha","age":35,"city":"Miami"}
```

ये उदाहरण सरल डेटा मैनिपुलेशन से लेकर जटिल API संचारों तक के कार्यों के लिए रूबी में JSON के साथ काम करने की सहज प्रकृति को दर्शाते हैं।
