---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:18.507785-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
सर्चिंग और रिप्लेसिंग टेक्स्ट का मतलब है, टेक्स्ट में से कुछ विशेष शब्दों या चरणों को चुनकर, उन्हें दूसरे शब्दों या चरणों से बदलना। प्रोग्रामर इसे डेटा को साफ़ करने, फिर से उपयोग में लाने, और गलतियों को सुधारने के लिए करते हैं।

## How to: (कैसे करें:)
यहाँ Gleam में एक साधारण उदाहरण दिया जा रहा है:

```Gleam
external fn regex_replace(original: String, pattern: String, replacement: String) -> String =
  "external_module" "regex_replace"

pub fn main() {
  let text = "फूल गुलाब का, खार क्या करे?"
  let updated_text = regex_replace(text, "गुलाब", "कमल")
  io.println(updated_text)  // इससे आउटपुट होगा: "फूल कमल का, खार क्या करे?"
}
```

## Deep Dive (गहराई से जानकारी)
सर्चिंग और रिप्लेसिंग टेक्स्ट का इतिहास कई दशकों पुराना है, जब से हमने कंप्यूटर पर टेक्स्ट संपादन शुरू किया था। सर्च और रिप्लेस फंक्शन हमें संग्रहित जानकारी में पैटर्न की पहचान और उन्हें बदलने की सुविधा देते हैं, जैसे की रेगुलर एक्सप्रेशन्स (RegExp)। ग्लिम में, यह कार्यक्षमता ऴेक्स्टर्नल मॉड्यूल्स के जरिए लागू की जाती है। विकल्पों में स्ट्रिंग फंक्शंस और विभिन्न पैकेजेस भी शामिल हैं जो सर्च और रिप्लेस टेक्स्ट को और भी अधिक सशक्त बनाते हैं।

## See Also (यह भी देखें)
- Regular expressions guide: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- External package for advanced text manipulation in Gleam: [https://hex.pm/packages?search=gleam](https://hex.pm/packages?search=gleam)