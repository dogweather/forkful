---
title:                "YAML के साथ काम करना"
aliases:
- /hi/fish-shell/working-with-yaml/
date:                  2024-02-03T19:26:00.437521-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML के साथ काम करना यानी YAML (YAML Ain't Markup Language) फ़ाइलों का पार्सिंग और हेरफेर करना, एक डेटा सीरियलाइजेशन फॉर्मेट जिसका उपयोग कॉन्फ़िगरेशन फ़ाइलों में किया जाता है, Fish Shell में। प्रोग्रामर यह नौकरियों को स्वचालित और कुशलतापूर्वक कॉन्फ़िगर करने के लिए करते हैं, जो शेल वातावरणों के संदर्भ में कॉन्फ़िगरेशन प्रबंधन और संसाधनों की प्रावधान की जैसी कार्यो को सुगम बनाता है।

## कैसे:
Fish Shell में YAML को पार्स करने के लिए अंतर्निहित समर्थन नहीं है, लेकिन आप `yq` (एक हल्का और पोर्टेबल कमांड-लाइन YAML प्रोसेसर) जैसे तृतीय-पक्ष उपकरण का उपयोग करके YAML डेटा को संभाल सकते हैं।

**yq की स्थापना (अगर पहले से स्थापित नहीं है):**
```fish
sudo apt-get install yq
```

**एक YAML फाइल से मान पढ़ना:**
मान लें आपके पास `config.yaml` नामक एक YAML फ़ाइल है जिसमें निम्नलिखित सामग्री है:
```yaml
database:
  host: localhost
  port: 3306
```

डेटाबेस होस्ट पढ़ने के लिए, आप उपयोग करेंगे:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**नमूना आउटपुट:**
```
localhost
```

**YAML फ़ाइल में एक मान अपडेट करना:**
`port` को `5432` में अपडेट करने के लिए, उपयोग करें:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**अपडेट की पुष्टि करें:**
```fish
yq e '.database.port' config.yaml
```
**नमूना आउटपुट:**
```
5432
```

**एक नई YAML फ़ाइल लिखना:**
पूर्वनिर्धारित सामग्री के साथ एक नई `new_config.yaml` बनाने के लिए:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
यह `yq` का उपयोग करके और सुंदर प्रिंट (-P फ्लैग) एक नई YAML फ़ाइल में एक स्ट्रिंग को प्रक्रिया करता है।

**जटिल संरचनाओं का पार्सिंग:**
अगर आपके पास एक अधिक जटिल YAML फ़ाइल है और आपको नेस्टेड अर्रेज़ या ऑब्जेक्ट्स निकालने की आवश्यकता है, तो आप:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**नमूना आउटपुट:**
```
server1
server2
```
`yq` का उपयोग करके, Fish Shell विभिन्न स्वचालन और कॉन्फ़िगरेशन कार्यों के लिए YAML दस्तावेज़ों के माध्यम से नेविगेट करना और उनको हेरफेर करना सरल बनाता है।
