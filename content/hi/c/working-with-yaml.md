---
title:                "YAML के साथ काम करना"
date:                  2024-01-19
simple_title:         "YAML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML का इस्तेमाल डाटा का सीरियलाइजेशन के लिए होता है, ज्यादातर कॉन्फिग फाइलों में। प्रोग्रामर्स इसे इस्तेमाल करते हैं क्योंकि यह समझने में आसान होता है और ह्यूमन-रीडेबल होता है।

## How to: (कैसे करें:)
आप C प्रोग्रामिंग भाषा में YAML फाइलों को पढ़ने और लिखने के लिए `libyaml` जैसे लाइब्रेरीज का इस्तेमाल कर सकते हैं।

```C
#include <stdio.h>
#include <yaml.h>

int main() {
    FILE *file = fopen("example.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    // YAML पार्सर शुरू करें
    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(file == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, file);

    // YAML इवेंट्स को पढ़ें
    while(1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        // यहाँ आप इवेंट के हिसाब से लॉजिक लिख सकते हैं

        if(event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    // पार्सर और फाइल क्लोज करें
    yaml_parser_delete(&parser);
    fclose(file);
    return 0;
}
```

यह साधारण कोड YAML फाइल से इवेंट्स रीड करता है।

## Deep Dive (गहराई से जानकारी)
YAML ("YAML Ain't Markup Language" या शुरुआती समय में "Yet Another Markup Language") डेटा सीरियलाइजेशन के लिए एक ह्यूमन-रीडेबल भाषा है। इसे 2001 में पेश किया गया था। JSON और XML इसके विकल्प हो सकते हैं, लेकिन YAML को ज्यादा पढ़ने में आसान माना जाता है। C में YAML को पार्स करने के लिए `libyaml` एक पोपुलर लाइब्रेरी है, जो इवेंट-आधारित पार्सिंग प्रदान करती है।

## See Also (इसे भी देखें)
- The Official YAML Website: [YAML](https://yaml.org)
- libyaml GitHub Repository: [libyaml](https://github.com/yaml/libyaml)
- YAML Syntax Overview: [Learn YAML in Y Minutes](https://learnxinyminutes.com/docs/yaml/)
