---
title:                "पाठ की खोज और प्रतिस्थापन"
aliases: - /hi/c/searching-and-replacing-text.md
date:                  2024-02-03T18:09:13.750670-07:00
model:                 gpt-4-0125-preview
simple_title:         "पाठ की खोज और प्रतिस्थापन"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में पाठ को खोजना और बदलना इसमें निहित विशेष उप-स्ट्रिंग की पहचान करने और उन्हें विभिन्न उप-स्ट्रिंग के साथ प्रतिस्थापित करने की प्रक्रिया है। प्रोग्रामर डेटा सैनिटाइजेशन और फॉर्मेटिंग से लेकर गतिशील रूप से सामग्री उत्पादन तक के कार्यों के लिए ये ऑपरेशन टेक्स्ट डेटा को संभालने हेतु करते हैं।

## कैसे करें:

C में स्ट्रिंग्स पर सीधे खोज और प्रतिस्थापन करने के लिए निर्मित फंक्शन्स उपलब्ध नहीं हैं। हालांकि, आप इसे `<string.h>` लाइब्रेरी में उपलब्ध विभिन्न स्ट्रिंग हैंडलिंग फंक्शन्स का संयोजन करके और कुछ कस्टम लॉजिक के साथ हासिल कर सकते हैं। नीचे एक साधारण उदाहरण दिया गया है कि कैसे एक स्ट्रिंग के भीतर एक उप-स्ट्रिंग को खोजकर उसे बदला जा सकता है। सरलता के लिए, यह उदाहरण पर्याप्त बफर आकार की मान्यता लेता है और स्मृति आवंटन समस्याओं को संभालना नहीं करता जिन्हें आपको उत्पादन कोड में विचार करना चाहिए।

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // मैच तक की लंबाई की गणना करें
        len_up_to_match = tmp - source;
        
        // मैच से पहले का भाग कॉपी करें
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // नया उप-स्ट्रिंग कॉपी करें
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // सोर्स स्ट्रिंग में मैच के पार आगे बढ़ें
        tmp += len_sub;
        source = tmp;
    }
    
    // सोर्स स्ट्रिंग का कोई भी शेष भाग कॉपी करें
    strcpy(insert_point, source);
    
    // संशोधित स्ट्रिंग प्रिंट करें
    printf("Modified string: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

नमूना आउटपुट:
```
Modified string: Hello, this is a sample. This sample is simple.
```

यह कोड एक साधारण दृष्टिकोण का प्रदर्शन करता है कि कैसे एक स्रोत स्ट्रिंग में एक उप-स्ट्रिंग (`sub`) की सभी उदाहरणों की खोज की जाए और उन्हें दूसरे उप-स्ट्रिंग (`newSub`) के साथ बदला जाए, जिसमें `strstr` फंक्शन का उपयोग हर मैच के प्रारम्भिक बिंदु को खोजने हेतु किया जाता है। यह एक बहुत ही साधारण उदाहरण है जो जटिल परिदृश्यों जैसे कि ओवरलैपिंग उप-स्ट्रिंग को संभाल नहीं पाता।

## गहराई से:

"कैसे करें" अनुभाग में प्रयुक्त दृष्टिकोण आधारभूत है, यह दिखाते हुए कि C में टेक्स्ट खोज और प्रतिस्थापन कैसे हासिल किया जा सकता है बिना किसी तृतीय-पक्ष लाइब्रेरी के। ऐतिहासिक रूप से, क्योंकि C का जोर निम्न-स्तरीय स्मृति प्रबंधन और प्रदर्शन पर है, इसकी स्टैण्डर्ड लाइब्रेरी उच्च-स्तरीय स्ट्रिंग मैनिपुलेशन कार्यक्षमताओं को encapsulate नहीं करती जैसे कि Python या JavaScript जैसी भाषाओं में पायी जाती हैं। प्रोग्रामर्स को मैनुअल रूप से स्मृति का प्रबंधन करना पड़ता है और वांछित परिणाम प्राप्त करने हेतु विभिन्न स्ट्रिंग ऑपरेशन्स का संयोजन करना पड़ता है, जो जटिलता में वृद्धि करता है लेकिन अधिक नियंत्रण और कुशलता प्रदान करता है।
