---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:27:08.694545-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, जिसका पूरा नाम YAML Ain't Markup Language है, Ruby में कॉन्फ़िगरेशन फाइलों और डेटा सीरियलाइज़ेशन के लिए व्यापक रूप से इस्तेमाल किया जाता है क्योंकि इसका फॉर्मेट मानव-पढ़ने योग्य होता है। जब प्रोग्रामर्स को डेटा ऑब्जेक्ट्स को संग्रहित करने या संचारित करने की जरूरत होती है, तो वे एक पढ़ने योग्य फिर भी संरचित तरीके में, YAML की ओर आकर्षित होते हैं, जो कि कॉन्फ़िगरेशन प्रबंधन, डेटा संग्रहण, और इंटर-लैंग्वेज डेटा साझाकरण जैसे कार्यों को सरल बनाता है।

## कैसे करें:
Ruby में Psych नामक एक बिल्ट-इन लाइब्रेरी आती है जो YAML को पार्स करने और उत्पन्न करने में मदद करती है। इसका इस्तेमाल करने के लिए, आपको पहले YAML स्टैंडर्ड लाइब्रेरी को रिक्वायर करना होगा। आपको शुरू करने के लिए एक बेसिक उदाहरण है:

```ruby
require 'yaml'

# हैश जिसे सीरियलाइज़ किया जाएगा
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# हैश को YAML में बदलना
yaml_data = person.to_yaml

puts yaml_data
```

**नमूना आउटपुट:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Ruby ऑब्जेक्ट में YAML डेटा को वापस लोड करने के लिए:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**नमूना आउटपुट:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### थर्ड-पार्टी लाइब्रेरियों का उपयोग:

यद्यपि मानक लाइब्रेरी सामान्य कार्यों के लिए पर्याप्त होती है, जटिल जरूरतों के लिए आप 'safe_yaml' जैसे थर्ड-पार्टी जेम्स की ओर देख सकते हैं। ऐसी लाइब्रेरियों का उपयोग करने के लिए, आपको पहले जेम इंस्टॉल करना होगा:

```bash
gem install safe_yaml
```

उसके बाद, आप इसका उपयोग करके YAML डेटा को सुरक्षित रूप से लोड कर सकते हैं, जैसे कि उपयोगकर्ता-नियंत्रित स्रोतों से ऑब्जेक्ट की संस्थापना जैसे जोखिमों को कम करना:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**नमूना आउटपुट:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

यह दृष्टिकोण आपके YAML हैंडलिंग की सुरक्षा को बढ़ाता है, जो कि अविश्वसनीय स्रोतों से YAML लोड करने वाले एप्लिकेशनों के लिए एक अच्छी पसंद है।
