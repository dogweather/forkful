---
title:                "Json के साथ काम करना"
html_title:           "Fish Shell: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप डेटा प्रसंस्करण, डाटा मॉडलिंग या डेटा इंजिनियरिंग क्षेत्र में काम करते हैं, तो आपने शायद ही JSON के बारे में सुना होगा। JSON (JavaScript Object Notation) आजकल डेटा को इंटरक्सिबल डेटा फॉर्मेट के रूप में सबसे लोकप्रिय है। Fish Shell में भी JSON को प्रोसेस करना बहुत आसान है। इस लेख में हम आपको Fish Shell में JSON का उपयोग करने का तरीका बताएंगे।

## कैसे करें

फिश शेल का प्रयोग जेसन संरचना के साथ JSON डेटा प्रसंस्करण करने के लिए किया जाता है। सबसे पहले, हमें जेसन डेटा को एक पैर्सर में पार्स करना होगा। पैर्सर्स हमेशा जेसन को वापस एक स्ट्रक्चर में तय करते हैं। उदाहरण के लिए:

```Fish Shell
set data (jq .users persons.json)
echo $data
```

यहां, हमने `jq` कमांड का उपयोग करके `persons.json` फ़ाइल को पार्स किया है और उसे `data` चयनित परिवर्तन में समायोजित किया है। फिर, हम `echo` कमांड का उपयोग करके अपने डेटा को प्रिंट कर सकते हैं।

अब, हम जेसन संरचना को एक वस्तु में सभी रों में बदलना चाहते हैं। इसके लिए, हम `for` लूप का उपयोग करके अपने डेटा के प्रत्येक पंक्ति को इटरेट करेंगे और इसे संशोधित वस्तु में सेट करेंगे। उदाहरण के लिए:

```Fish Shell
for person in $data
	set person.name ($person.name | tr '[:upper:]' '[:lower:]')
end
echo $data
```

यहां, हमने `for` लूप का उपयोग करके `data` फ़ाइल के प्रत्येक पंक्ति को `person` चयनित परिवर्तन में स