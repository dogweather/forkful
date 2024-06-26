---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:14.569558-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C \u092E\u0947\u0902\
  \ JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A \u0906\u092E\u0924\u094C\u0930 \u092A\
  \u0930 `jansson` \u092F\u093E `json-c` \u091C\u0948\u0938\u0940 \u0932\u093E\u0907\
  \u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0947\u0902\u0917\u0947 \u0915\u094D\u092F\u094B\u0902\u0915\u093F\
  \ C \u092E\u0947\u0902 JSON \u0915\u0947 \u0932\u093F\u090F \u0928\u093F\u0930\u094D\
  \u092E\u093F\u0924 \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:53.183815-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A \u0906\u092E\
  \u0924\u094C\u0930 \u092A\u0930 `jansson` \u092F\u093E `json-c` \u091C\u0948\u0938\
  \u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0917\u0947 \u0915\u094D\u092F\
  \u094B\u0902\u0915\u093F C \u092E\u0947\u0902 JSON \u0915\u0947 \u0932\u093F\u090F\
  \ \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0938\u092E\u0930\u094D\u0925\u0928\
  \ \u0915\u093E \u0905\u092D\u093E\u0935 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901\
  , \u0939\u092E `jansson` \u092A\u0930 \u0927\u094D\u092F\u093E\u0928 \u0915\u0947\
  \u0902\u0926\u094D\u0930\u093F\u0924 \u0915\u0930\u0947\u0902\u0917\u0947 \u0907\
  \u0938\u0915\u0940 \u0909\u092A\u092F\u094B\u0917 \u092E\u0947\u0902 \u0906\u0938\
  \u093E\u0928\u0940 \u0914\u0930 \u0938\u0915\u094D\u0930\u093F\u092F \u0930\u0916\
  -\u0930\u0916\u093E\u0935 \u0915\u0947 \u0915\u093E\u0930\u0923\u0964 \u092A\u0939\
  \u0932\u0947, \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u094B\
  \ \u0938\u094D\u0925\u093E\u092A\u093F\u0924 \u0915\u0930\u0947\u0902 (\u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0915\u0947 \u0932\u093F\u090F, Ubuntu \u092A\u0930 `apt`\
  \ \u091C\u0948\u0938\u0947 \u092A\u0948\u0915\u0947\u091C \u092E\u0948\u0928\u0947\
  \u091C\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  ."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
C में JSON के साथ काम करने के लिए, आप आमतौर पर `jansson` या `json-c` जैसी लाइब्रेरी का उपयोग करेंगे क्योंकि C में JSON के लिए निर्मित समर्थन का अभाव है। यहाँ, हम `jansson` पर ध्यान केंद्रित करेंगे इसकी उपयोग में आसानी और सक्रिय रख-रखाव के कारण। पहले, लाइब्रेरी को स्थापित करें (उदाहरण के लिए, Ubuntu पर `apt` जैसे पैकेज मैनेजर का उपयोग करके: `sudo apt-get install libjansson-dev`).

आइए एक JSON स्ट्रिंग को पार्स करना और उसकी सामग्री तक पहुँचना शुरू करें:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Name: %s\nAge: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

नमूना आउटपुट:
```
Name: John Doe
Age: 30
```

आगे, एक JSON ऑब्जेक्ट बनाना और उसे आउटपुट करना:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

नमूना आउटपुट:
```
{"name": "Jane Doe", "age": 25}
```

ये उदाहरण एक JSON स्ट्रिंग को लोड करने, इसके मूल्यों को अनपैक करने, एक नया JSON ऑब्जेक्ट बनाने, और फिर इसे स्ट्रिंग के रूप में आउटपुट करने की मूल बातों का प्रदर्शन करते हैं।

## गहराई में
C में JSON के साथ काम करने की आवश्यकता वेब के JSON को डेटा इंटरचेंज के लिए प्राथमिक प्रारूप के रूप में अपनाने से उत्पन्न हुई है। JSON की सादगी और कार्यक्षमता ने इसे XML को जल्दी से प्रतिस्थापित कर दिया, भले ही C में शुरू में JSON संचालन के लिए सीधा समर्थन न हो। प्रारंभिक समाधानों में मैनुअल स्ट्रिंग मैनिपुलेशन शामिल था - जो की त्रुटि-प्रवण और अकुशल था। `jansson` और `json-c` जैसी लाइब्रेरी इस अंतराल को भरने के लिए सामने आईं, JSON पार्सिंग, निर्माण और सीरियलाइज़ेशन के लिए मजबूत APIs प्रदान करती हैं।

`jansson` जहां आसानी और उपयोग में सादगी प्रदान करता है, `json-c` उन लोगों के लिए अपील कर सकता है जो व्यापक फीचर सेट की तलाश में हैं। फिर भी, C++ में पार्सिंग लाइब्रेरीज जैसे विकल्प अधिक परिष्कृत अमूर्तताएं प्रदान करते हैं, धन्यवाद उस भाषा की अधिक जटिल डेटा संरचनाओं और मानक पुस्तकालय समर्थन के लिए। हालाँकि, जब ऐसे वातावरण में काम किया जाता है जहाँ C पसंदीदा या आवश्यक भाषा होती है - जैसे कि एम्बेडेड सिस्टम्स में या मौजूदा C लाइब्रेरीज के साथ इंटरफेसिंग करते समय - `jansson` या `json-c` का उपयोग अनिवार्य हो जाता है।

यह भी उल्लेखनीय है कि C में JSON के साथ काम करना स्मृति प्रबंधन की एक गहरी समझ में शामिल है, क्योंकि ये लाइब्रेरी अक्सर गतिशील रूप से आवंटित ऑब्जेक्ट्स को लौटाती हैं जिन्हें स्पष्ट रूप से मुक्त करने की आवश्यकता होती है। यह प्रोग्रामरों को स्मृति रिसाव को रोकने की महत्वपूर्ण आवश्यकता के साथ सुविधा के संतुलन बनाने की चुनौती देता है, C कोड तैयार करने का एक महत्वपूर्ण पहलू है।
