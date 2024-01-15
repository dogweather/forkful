---
title:                "Json के साथ काम करना"
html_title:           "C: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON डेटा प्रोसेसिंग को करने से पहले, यह जानना जरूरी है कि इसका इस्तेमाल डेटा को आसान और संरचित रूप में संगठित करने के लिए होता है। यह डेटा इंटरएक्शन को सरल बनाने के लिए एक प्रैसेसिंग टूल के रूप में भी काम करता है।

## कैसे करें

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main(){
  // डेटा बनाएं
  json_object *sample_data = json_object_new_object();
  json_object *name = json_object_new_string("John Doe");
  json_object *age = json_object_new_int(25);
  json_object *is_married = json_object_new_boolean(false);

  // डेटा को प्रिंट करें
  printf("नाम: %s \nउम्र: %d \nशादीशुदा: %s", json_object_get_string(name), 
                                        json_object_get_int(age), 
                                        json_object_get_boolean(is_married) ? "हां": "नहीं");

  // डेटा को फ्री करें
  json_object_put(name);
  json_object_put(age);
  json_object_put(is_married);
  json_object_put(sample_data);

  return 0;
}
```

आप ऊपर दिए गए कोड ब्लॉक को कॉपी-पेस्ट करके, एक प्रोग्राम चला सकते हैं जो नाम, उम्र और शादीशुदा होने की स्थिति के साथ एक्सट्रैक्ट किए गए जेसन डेटा को प्रिंट करेगा।

## गहराई में समझें

जब आप जेसन दो या अधिक ऑब्जेक्ट्स को कम्पाइल करते हैं, तो आपको जेसन कोडिंग प्रणाली को समझने की आवश्यकता होती है। जेसन-सी बाइंडिंग आपको एक प्रोग्रामिंग भाषा की तरह डेटा को स्ट्रक्चरली बनाएं और एक पहनावा बेंट को इंटरफेस करता है। इसमें, आप जेसन जावास्क्रिप्ट से भिन्न होते हैं जो ब्राउज़र या वेबसाइट में उपयोग किया जाता है।

## इस आलेख को और भी देखें

- [JSON-सी गाइड](https://github.com/json-c/json-c/wiki)
- [ज