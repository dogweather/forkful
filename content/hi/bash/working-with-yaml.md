---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
YAML एक डेटा सीरियलाइजेशन फॉर्मेट है, जो कॉन्फ़िगरेशन फ़ाइलों और डेटा मैसेजिंग के लिए उपयोग होता है। प्रोग्रामर्स इसे आसानी से पढ़ने और लिखने के लिए, और क्रॉस-लैंग्वेज डेटा शेयरिंग के लिए उपयोग करते हैं।

## कैसे करें? (How to:)
Bash में YAML के साथ काम करना यानि `yq` और `jq` जैसे टूल्स का उपयोग करना, इनकी मदद से आप YAML फ़ाइलें पढ़ और बदल सकते हो।

```Bash
# YAML फ़ाइल की सामग्री पढ़ें
echo "name: Navin" > example.yaml
echo "role: Developer" >> example.yaml

# 'yq' का उपयोग करके फ़ाइल की सामग्री प्रिंट करें
yq e '.name' example.yaml
```
आउटपुट:
```
Navin
```

## गहराई में (Deep Dive)
YAML ("YAML Ain't Markup Language") की शुरुआत 2001 में हुई थी। इसके विकल्प के रूप में JSON और XML हैं। Bash में YAML के साथ काम करते समय, `yq` (YAML प्रोसेसिंग टूल जो `jq` के इंटरफ़ेस की नकल करता है) और `jq` का उपयोग सबसे ज्यादा होता है। ये टूल्स आपको YAML डेटा के साथ पाइपलाइन ऑपरेशंस करने और जटिल क्वेरीज़ चलाने की अनुमति देते हैं।

## और भी देखें (See Also)
- YAML की आधिकारिक वेबसाइट: [The Official YAML Website](https://yaml.org)
- `yq` और `jq` टूल्स के डॉक्युमेंटेशन: [yq GitHub repository](https://github.com/kislyuk/yq), [jq Official documentation](https://stedolan.github.io/jq/manual/)
- YAML के साथ काम करने के लिए उपयोगी Bash स्क्रिप्टिंग टिप्स: [Bash Scripting Cheatsheet](https://devhints.io/bash)
