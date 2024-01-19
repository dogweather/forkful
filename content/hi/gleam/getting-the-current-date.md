---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

'वर्तमान तारीख' प्राप्त करना - यह वह संघटन है, जबकि प्रोग्रामर आवश्यकता अनुसार वर्तमान काल के विवरण को प्राप्त करते हैं। यह तारीख और समय संग्रहीत करने, डेटा को ट्रैक करने और लॉग बनाने की उपयोगिता में आती है।

## कैसे?

वर्तमान में, Gleam में 'वर्तमान तारीख' के लिए कोई मूल फ़ंक्शन नहीं है। हालांकि, इरलैंग का `:calendar.universal_time/0` फ़ंक्शन का उपयोग करके इसका समाधान किया जा सकता है। 

```Gleam
import erlang

fn get_current_date() {
  erlang.(:calendar.universal_time())
}
```

जब आप इसे चलाते हैं, आपको वर्तमान यूनिवर्सल टाइम मिलेगा।

## गहरियाँ

1. **ऐतिहासिक प्रकटी:** वर्तमान दिनांक / समय का पता लगाना कंप्यूटर प्रोग्रामर का एक पुराना अभ्यास है जिसे डेटा की टाइम स्टैम्पिंग, डेटा ट्रैकिंग, और समाधान देखने के लिए उपयोग किया जाता है।
2. **विकल्प:** आप `:erlang.localtime/0` फ़ंक्शन का उपयोग भी कर सकते हैं जो व्यवस्था के क्षेत्रीय समय के अनुसार वर्तमान समय की जानकारी प्रदान करते हैं।
3. **लागू करने का विवरण:** इरलैंग के :calendar.universal_time() फ़ंक्शन का उपयोग Gleam में वर्तमान तारीख प्राप्त करने के लिए किया जाता हैं।

## यह भी देखें

1. [Erlang Documentation - Time and Date](https://erlang.org/doc/apps/erts/time_correction.html)
2. [Gleam Documentation - Interoperability](https://gleam.run/book/tour/interop.html)