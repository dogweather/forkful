---
title:                "YAML के साथ काम करना"
aliases:
- /hi/python/working-with-yaml/
date:                  2024-02-03T19:27:31.657235-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, जिसका पूरा नाम है YAML Ain't Markup Language, एक मानव-पठनीय डेटा सीरियलाइजेशन प्रारूप है। प्रोग्रामर YAML का उपयोग कॉन्फिगरेशन फाइलों, इंटर-प्रोसेस संदेशन और डेटा संग्रहण के लिए करते हैं क्योंकि इसकी सरल सिंटैक्स और अन्य प्रारूपों जैसे कि XML या JSON की तुलना में इसे पढ़ना आसान होता है।

## कैसे:
Python में YAML को पढ़ने और लिखने के लिए आमतौर पर एक तृतीय-पक्ष पुस्तकालय का उपयोग होता है, `PyYAML` सबसे लोकप्रिय होता है। शुरू करने के लिए, आपको PyYAML को स्थापित करने के लिए `pip install PyYAML` चलाना होगा।

**उदाहरण: एक YAML फ़ाइल में लिखना**

```python
import yaml

डेटा = {'एक सूची': [1, 42, 3.141, 1337, 'मदद', u'€'],
        'एक स्ट्रिंग': 'बू!',
        'एक अन्य डिक्शनरी': {'फू': 'बार', 'की': 'मान', 'उत्तर': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(डेटा, f, default_flow_style=False)

# इससे `example.yaml` फ़ाइल बनती है जिसमें डेटा YAML प्रारूप में संरचित होता है।
```

**उदाहरण: एक YAML फ़ाइल से पढ़ना**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# आउटपुट: 
# {'एक सूची': [1, 42, 3.141, 1337, 'मदद', '€'],
#  'एक स्ट्रिंग': 'बू!',
#  'एक अन्य डिक्शनरी': {'फू': 'बार', 'की': 'मान', 'उत्तर': 42}}
```

**कॉन्फिगरेशन के लिए YAML का उपयोग करना**

बहुत से प्रोग्रामर YAML का उपयोग एप्लिकेशन कॉन्फिगरेशन्स को प्रबंधित करने के लिए करते हैं। यहां एक उदाहरण दिया गया है कि कैसे किसी कॉन्फिग फ़ाइल को स्ट्रक्चर किया जा सकता है और इसे पढ़ा जा सकता है:

config.yaml:
```yaml
database:
  होस्ट: localhost
  पोर्ट: 5432
  उपयोगकर्ता_नाम: admin
  पासवर्ड: secret
```

Python में कॉन्फिग फ़ाइल को पढ़ना:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['होस्ट'])  # आउटपुट: localhost
```

**जटिल संरचनाओं को संभालना**

जटिल संरचनाओं के लिए, PyYAML आपको कस्टम पायथन ऑब्जेक्ट्स को परिभाषित करने की अनुमति देता है। हालाँकि, मनमाने फ़ंक्शंस या ऑब्जेक्ट्स को निष्पादित करने से बचने के लिए `safe_load` का उपयोग करके सुरक्षित प्रथाओं को सुनिश्चित करें।

```python
import yaml

# एक पायथन ऑब्जेक्ट परिभाषित करें
class Example:
    def __init__(self, मान):
        self.मान = मान

# कस्टम कंस्ट्रक्टर
def constructor_example(loader, node):
    मान = loader.construct_scalar(node)
    return Example(मान)

# टैग "!example" के लिए कंस्ट्रक्टर जोड़ें
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'डेटा'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.मान)  # आउटपुट: डेटा
```

इस स्निपेट में, `!example` एक कस्टम टैग है जो एक YAML स्ट्रिंग से 'डेटा' मान के साथ एक `Example` ऑब्जेक्ट को इंस्टैंटिएट करता है। ऐसे कस्टम लोडर PyYAML की लचीलेपन को बढ़ाते हैं, जिससे अधिक जटिल डेटा संरचनाओं और प्रकारों को संसाधित करना संभव होता है।
