---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जेसन (JSON) एक डेटा फॉरमेट है जो डेटा को स्टोर करने और नेटवर्क पर डेटा भेजने के लिए इस्तेमाल होता है। प्रोग्रामर्स इसका उपयोग API और वेबसर्विसेज से डेटा एक्सचेंज में वरीयता देते हैं क्योंकि यह लाइटवेट और आसानी से पढ़ने योग्य है।

## How to:
```C
// C JSON प्रोसेसिंग के लिए cJSON लाइब्रेरी का उदाहरण

#include <stdio.h>
#include "cJSON.h"

int main() {
    // JSON स्ट्रिंग का उदाहरण
    const char *json_string = "{\"name\":\"राज\",\"age\":30}";

    // JSON पार्स करना
    cJSON *root = cJSON_Parse(json_string);

    // JSON ऑब्जेक्ट से डेटा निकालना
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(root, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(root, "age");

    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("नाम: %s\n", name->valuestring);
    }

    if (cJSON_IsNumber(age)) {
        printf("उम्र: %d\n", age->valueint);
    }

    // JSON ऑब्जेक्ट को रिलीज करना
    cJSON_Delete(root);

    return 0;
}

```
उपरोक्त उदाहरण में JSON स्ट्रिंग को पार्स किया गया और उसमें से नाम और उम्र की जानकारी निकाली गई।

## Deep Dive (गहन अध्ययन)
JSON (JavaScript Object Notation) एक टेक्स्ट-बेस्ड डेटा फॉरमेट है जो 2001 में प्रसिद्ध हुआ। XML और YAML इसके विकल्प हैं लेकिन JSON अधिक सामान्य है क्योंकि यह हल्का और कम जटिल है। इसे पार्स करने और उत्पन्न करने के लिए C में cJSON और Jansson जैसी लाइब्रेरीज़ उपलब्ध हैं।

## See Also (और देखें)
- cJSON GitHub Repository: https://github.com/DaveGamble/cJSON
- JSON.org वेबसाइट, JSON स्पेसिफिकेशन और उदाहरण: http://json.org
- नेटवर्किंग और JSON: https://beej.us/guide/bgnet/html/#json
- प्रोग्रामिंग विद JSON: http://www.json-tutorial.com/
