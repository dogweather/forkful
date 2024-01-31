---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:20:31.462968-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML एक डेटा सीरियलाइजेशन भाषा है जिसे पढ़ने और लिखने में आसान बनाया गया है। प्रोग्रामर इसका उपयोग कॉन्फ़िग फ़ाइलों, सरल डेटा स्टोरेज, और क्रॉस-लैंग्वेज डेटा एक्सचेंज के लिए इसकी स्पष्टता और मानव-मित्रता के कारण करते हैं।

## कैसे करें:
आइए C में "tomlc99" लाइब्रेरी का उपयोग करके एक TOML कॉन्फ़िग फ़ाइल का पार्स करें। पहले, लाइब्रेरी को इंस्टॉल करें। तब, एक `config.toml` बनाएं:

```toml
title = "TOML उदाहरण"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

अब, इसे C में पार्स करें:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("गलती: कॉन्फ़िग फ़ाइल खोल नहीं सकता\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("गलती: %s\n", errbuf);
        return 1;
    }

    printf("शीर्षक: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("मालिक का नाम: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
नमूना आउटपुट:
```
शीर्षक: "TOML उदाहरण"
मालिक का नाम: "Tom Preston-Werner"
```

## गहराई से विवेचन
TOML, जिसे Tom's Obvious, Minimal Language के लिए खड़ा किया गया है, 2013 में Tom Preston-Werner द्वारा बनाया गया था। यह XML और YAML जैसे प्रारूपों के मुकाबले एक सरल विकल्प के रूप में कार्य करता है, अधिक मानव-पठनीय और लिखने योग्य होने पर ध्यान केंद्रित करता है। जबकि JSON एक और विकल्प है, TOML मानव द्वारा दृश्यरूप से पार्स करने में आसान एक संरचना को बनाए रखता है, जो कॉन्फ़िग फ़ाइलों में इसके अपनाने के प्राथमिक कारणों में से एक है।

C में, TOML के साथ काम करना किसी पार्सर लाइब्रेरी को चुनने की आवश्यकता होती है क्योंकि भाषा में इसे स्वाभाविक रूप से समर्थन नहीं मिलता है। "tomlc99" जैसी लाइब्रेरियां C99 के अनुरूप हैं और TOML पाठ को डिकोड करने के लिए एक API प्रदान करती हैं। प्रदर्शन पर विचार करते समय, उचित त्रुटि संभाल और स्मृति प्रबंधन महत्वपूर्ण होते हैं क्योंकि C में बिल्ट-इन गार्बेज कलेक्शन नहीं होता है।

## देखें:
1. TOML विनिर्देश: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub रेपो: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. डेटा सीरियलाइजेशन प्रारूपों की तुलना: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
