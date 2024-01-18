---
title:                "Yaml के साथ काम करना"
html_title:           "Lua: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?

YAML काम क्या है और क्यों होता है? यह प्रोग्रामर्स द्वारा उपयोग किया जाने वाला एक कॉन्फ़िगरेशन डाटा प्रोटोकॉल है जो कंप्यूटर प्रोग्राम्स को इनपुट और आउटपुट डेटा के साथ काम करने की अनुमति देता है।

## कैसे करें:
```Lua
-- YAML फ़ाइल को लोड करें
local yaml = require("yaml")

-- डेटा को YAML में मान दें
local data = {
    name = "जॉन डो",
    उम्र = 30,
    interests = {"गाना गाना", "क्रिकेट खेलना"}
}

-- डेटा को YAML में कन्वर्ट करें
local yaml_data = yaml.dump(data)

-- YAML डेटा को प्रिंट करें
print(yaml_data)
```

आउटपुट:
```yaml
name: जॉन डो
उम्र: 30
interests:
  - गाना गाना
  - क्रिकेट खेलना
```

## गहराई में जानें:
YAML स्थापित और मेंटेनेड द्वारा प्रोग्रामिंग भाषाओं में समर्थित है। यह अन्य कॉन्फ़िगरेशन डाटा प्रोटोकॉल्स के मुकाबले सटीकता और पढ़ने में अधिक सादाता प्रदान करता है। इसका उपयोग प्रोग्रामिंग और वेब डेवलपमेंट में डेटा समूह और कॉन्फ़िगरेशन को सभी में रखने के लिए किया जाता है।

## अन्य स्रोत देखें:
- [Lua विकिपीडिया पृष्ठ](https://hi.wikipedia.org/wiki/Lua)
- [YAML विकिपीडिया पृष्ठ](https://hi.wikipedia.org/wiki/YAML)