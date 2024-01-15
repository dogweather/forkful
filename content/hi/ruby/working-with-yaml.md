---
title:                "यामल के साथ काम करना"
html_title:           "Ruby: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यामल के साथ काम करने का *क्यों* कोई व्यक्ति करना चाहेंगा वह दो से अधिक वक्रोति नहीं है। यामल फाइल संरचना में स्पष्टता और सुव्यवस्थित कोडिंग के साथ काम करने का सुविधाजनक सरल तरीका प्रदान करता है। यह भी कम समय लेता है अपने प्रोजेक्ट को अपडेट करने के लिए सामग्री और संरचना बदलने को।

## कैसे

```Ruby
require 'yaml'

# YAML फ़ाइल लोड करें
file = YAML.load_file("data.yaml")

# प्रिंटफ़ एक्सम्पल डाउनलोड करें
puts file["name"]

# YAML फ़ाइल में डाटा बदलें
file["age"] = 25

# YAML फ़ाइल में डाटा सेव करें
File.open("data.yaml", "w") { |f| f.write file.to_yaml }

# नया फ़ाइल डाउनलोड करें
new_file = YAML.load_file("data.yaml")

# संशोधित डाटा प्रिंट करें
puts new_file["age"]
```

*आउटपुट:*
John Doe
25

## डीप डाइव

YAML कोडिंग में कुछ जरूरी बातें हैं जो आपको ध्यान में रखना चाहिए। फ़ाइल को संशोधित करने से पहले इसे लोड करना बेहद जरूरी है। एक फ़ाइल में उन्हें संशोधित करने के लिए दो एक के साथ जुड़ सकते हैं। परिवर्तन के बाद, फ़ाइल को लिखना न भूलें नागम्य संस्थान से परिवर्तन दर्शक बाद में स्वस्थ फ़ाइल संरचना दिखाई देगी।

## देखें

[Official YAML documentation](https://yaml.org/spec/1.2/spec.html)
[YAML Tutorial for Beginners](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)
[YAML Cheatsheet](https://yaml.org/refcard.html)