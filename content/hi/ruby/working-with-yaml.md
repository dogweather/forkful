---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक डेटा सीरियलाइज़ेशन फॉर्मेट है जो कि मानव-पठनीय होता है। प्रोग्रामर्स डेटा को आसानी से स्टोर और पढ़ने, विन्यास (configuration) फाइलों को संभालने और ऑब्जेक्ट्स को सीरियलाइज़ या डिसीरियलाइज़ करने के लिए YAML का इस्तेमाल करते हैं।

## How to: (कैसे?)
### YAML फाइल पढ़ना:
```Ruby
require 'yaml'

# YAML फाइल लोड करें
config = YAML.load_file('example.yml')

# लोडेड डेटा का प्रयोग
puts config['greeting']
```

संभावित `example.yml` फाइल का कंटेंट:
```yaml
greeting: नमस्कार, YAML का उपयोग कैसे करें?
```

संभावित आउटपुट:
```
नमस्कार, YAML का उपयोग कैसे करें?
```

### YAML फाइल में डेटा लिखना:
```Ruby
require 'yaml'

# डेटा को हैश के रूप में संग्रहित करना
config = {'goodbye' => 'अलविदा, फिर मिलेंगे!'}

# YAML फाइल में डेटा लिखना
File.open('example.yml', 'w') {|f| f.write(config.to_yaml) }
```

अब `example.yml` में लिखा होगा:
```yaml
---
goodbye: अलविदा, फिर मिलेंगे!
```

## Deep Dive (गहराई से जानकारी)
YAML, जिसे 2001 में पेश किया गया, XML और JSON के लिए एक सरल विकल्प के तौर पर उभरा। इसका मुख्य लक्ष्य पठनीयता और आसानी से समझ में आने वाले डेटा प्रदर्शन की पेशकश करना है। Ruby में YAML को हैंडल करने के लिए 'Psych' लाइब्रेरी सबसे आम है, जिसे Ruby के स्टैंडर्ड लाइब्रेरी में बंडल किया गया है। आप 'SafeYAML' जैसे विकल्पों का भी प्रयोग कर सकते हैं, जो सुरक्षा पहलुओं पर जोर देते हैं। YAML सिंटैक्स में इंडेंटेशन महत्वपूर्ण होता है और यह डेटा संरचनाओं को पेश कर सकता है जैसे मैप्स, स्कालर्स, और सेक्वेन्स। 

## See Also (और देखें)
- [YAML ऑफिशियल साइट](https://yaml.org/)
- [Ruby YAML प्रलेखन](https://ruby-doc.org/stdlib-3.1.0/libdoc/yaml/rdoc/YAML.html)
- [Ruby में सेफ YAML डेसीरियलाइजेशन](https://blog.bigbinary.com/2020/08/04/ruby-yaml-load.html)
- [Psych लाइब्रेरी कोड](https://github.com/ruby/psych)
