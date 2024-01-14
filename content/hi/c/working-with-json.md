---
title:                "C: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-json.md"
---

{{< edit_this_page >}}

प्रयोक्ताओं के लिए जेसन (JSON) काम करने के कारण क्या है? 

जेसन (JSON) को संरचित करने और साबित करने के लिए दो तरह का प्रयोग किया जा सकता है। पहले, आप इसे एक कॉड ग्रीष्म (code snippets) के साथ सीख सकते हैं। इसके अलावा, जेसन (JSON) को ऐसे निर्देश तथा कार्य प्रस्तुत करते हैं जिससे आपको यह समझने में सहायता मिलेगी कि आप इसका प्रयोग कैसे करें। 

## क्यों:

जेसन (JSON) एक आसान तरीके से पाठों और संरचनाओं को संगठित करने में मदद करता है। यह कई भाषाओं और विभिन्न एप्लिकेशन के साथ अपनी सामान्य तरीके से संयोजित होने के लिए सुविधाएं प्रदान करता है। इसलिए, यह आमतौर पर प्रोग्रामिंग के जरिए डेटा को पढ़ने और लिखने के लिए प्रयोग किया जाता है।

## कैसे करें:

```C
#include <stdio.h>
#include <stdlib.h>
#include <json.h>

int main() {
    // Create a sample JSON object
    char* json_string = "{\"name\": \"Hindi Reader\"}";

    // Parse the JSON string
    json_object *obj = json_tokener_parse(json_string);

    // Get the value of the "name" key
    json_object *name;
    json_object_object_get_ex(obj, "name", &name);

    // Print the value
    printf("%s is learning about JSON!", json_object_get_string(name));

    // Free the memory
    json_object_put(obj);

    return 0;
}
```

आप ऊपर दिए गए कोड ग्रीष्म में देख सकते हैं कि हमने कैसे एक JSON स्ट्रिंग को बनाया, उसे उसे पार्स किया और उसमें से डेटा को प्राप्त किया। इस तरह से, आप भी अपने कोड में जेसन (JSON) का प्रयोग कर सकते हैं।

## गहराई में खोज करें:

जेसन (JSON) में कुछ महत्वपूर्ण बातें जानने के लिए आप इसकी गहराई में खोज कर सकते हैं। यह एक लो