---
title:                "C: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

यम्ल क्या है और क्यों आपको इसमें काम करना चाहिए?

यम्ल (YAML) एक पाठ के डेटा फॉर्मेट है जो पेशेवरों के लिए एक बहुत ही उपयोगी और अव्यवस्थित स्ट्रक्चर है। इसका उपयोग डेटा को संग्रहीत करने, पार्सिंग करने और समायोजित करने के लिए किया जाता है। यम्ल में डेटा को प्रस्तुत करने का नियंत्रण आसान होता है और इसे हमेशा मानचित्रों और टेबल में प्रस्तुत करना सरल होता है। यम्ल फाइल्स को बनाना और संपादित करना भी बहुत आसान होता है। इसलिए, यम्ल को समझने और सी प्रोग्रामिंग में उसका उपयोग करने के लिए बहुत ही लाभदायक हो सकता है।

कैसे करें: यम्ल के साथ कैसे काम करें?

```C
#include <yaml.h>

int main() {

  // एक नया यम्ल डॉक्यूमेंट बनाएं
  yaml_document_t document;
  yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

  // नया स्केलर नोड संयोजित करें
  yaml_node_t *node = yaml_document_add_scalar(&document, NULL, (yaml_char_t *)"नमस्ते दुनिया", 10, YAML_PLAIN_SCALAR_STYLE);

  // यम्ल डॉक्यूमेंट को फाइल में लिखें
  FILE *outfile = fopen("hello_world.yaml", "w");
  yaml_document_dump(outfile, &document);

  // यम्ल डॉक्यूमेंट को मुक्त करें
  yaml_document_delete(&document);
  fclose(outfile);

  return 0;
}
```

यहां आप एक नए यम्ल डॉक्यूमेंट को कैसे बनाएंगे, स्केलर नोड को कैसे संयोजित करेंगे और फाइल में उन्हें लिखेंगे। यहां हमने "नमस्ते दुनिया" शब्द को स्केलर नोड के माध्यम से जोड़ा है। आप इसी तरह से अन्य डेटा टाइप्स भी जोड़ सकते ह