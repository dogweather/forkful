---
title:                "Elixir: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

येमल (YAML) एक पाठ दस्तावेज़ है जो डेटा को एक संरचित और पढ़ने में आसान रूप में प्रस्तुत करता है। Elixir प्रोग्रामिंग भाषा में YAML का इस्तेमाल करना एक बहुत ही सुविधाजनक और आसान होता है और यह डेटा को कॉन्फिगर करने और संभालने के लिए भी बहुत ही अच्छा विकल्प है।

## कैसे करें

येमल को Elixir में इस्तेमाल करने के लिए, हम येमल लाइब्रेरी को अपने प्रोजेक्ट में जोड़ सकते हैं। फिर hm_yaml मॉड्यूल का उपयोग करके, हम येमल फ़ाइल से डेटा पढ़ सकते हैं। नीचे एक उदाहरण दिया गया है:

```Elixir
yaml_string = """
  name: John
  age: 30
  hobbies:
    - hiking
    - cooking
"""
yaml_data = HMYaml.load_string(yaml_string)
```

यहां, हमने एक येमल स्ट्रिंग बनाई है और उसे HMYaml.load_string() फ़ंक्शन का उपयोग करके लोड किया है। यहां हमें डेटा का एक मैप मिल गया है जिसमें हमारे द्वारा निर्धारित की गई संपूर्ण जानकारी है। हम मैप के कुंजी के साथ सीधे एक्सेस कर सकते हैं जैसे yaml_data["name"] और उसके लिए उनके मूल्य मिलेंगे जैसे "John"।

अधिक विस्तृत उदाहरणों के लिए, आप [येमल लाइब्रेरी डॉक्यूमेंटेशन](https://hexdocs.pm/hm_yaml/readme.html) पर जा सकते हैं।

## गहरी खोज

येमल ऑफिशियली YAML Ain't Markup Language को आधार बनाकर बनाया गया है। यह डेटा को हमारी भाषा से अलग संरचित करने के लिए आसान बनाता है जो हमारे प्रोजेक्ट में कॉन्फ