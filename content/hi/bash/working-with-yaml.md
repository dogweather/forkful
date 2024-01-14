---
title:                "Bash: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

आज के डिजिटल युग में, प्रोग्रामिंग का अपना एक अलग ही महत्व है। और इसके साथ-साथ मशीनों के साथ काम करने के लिए ऐसे फाॅर्मेटों की भी जरूरत होती है जो हमारे व्यंग सोच के साथ साथ, मशीनों के लिए भी सहज हो जिससे की हमें हमारे काम को अपने हमजोली में पूरा करने में आसानी हो। और इस काम में YAML, एक पावरफुल और स्कलेबल भाषा है जो के रूप में हमारे काम को और भी ज्यादा सहज बना देता है। इसलिए आप भी यमल के साथ काम करना शुरू कर सकते हैं!

## कैसे करें

यमल हमें हमारी डेटा को स्ट्रक्चर के रूप में संग्रहीत करने की सावधानी करती है। उदाहरण के लिए, हम इस तरह से YAML फ़ाइल को बना सकते हैं: 

```Bash
# उदाहरण फ़ाइल बनाने की आवश्यक लाइन 
vim example.yaml 

# डेटा का स्ट्रक्चर पेश करने के लिए इस तरह से YAML फ़ाइल में कोड करें
metadata:
  name: John Doe
  profession: Programmer
address:
  city: New Delhi
  country: India
```

इसके बाद हम कोड को रन कर सकते हैं और स्ट्रक्चर को देख सकते हैं: 

```Bash
# यमल को रन करने के लिए कमांड 
python example.yaml 

# स्ट्रक्चर को देखने के लिए यमल को रन करने 
metadata:
  name: John Doe
  profession: Programmer
address:
  city: New Delhi
  country: India 
```

स्वतंत्रता के साथ, हम अपने स्ट्रक्चर को डिकोड भी कर सकते हैं। उदाहरण के लिए: 

```Bash
# स्वतंत्र रूप से देखते हैं
data=$(cat example.yaml)
echo "$data" 

# स्ट्रक्चर को डि सी ओडिंग करते हैं 
python -c "from yaml import load; import sys; print(load(sys.stdin))" < example.yaml 

# स्ट्रक्चर को स