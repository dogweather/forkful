---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T17:59:37.222721-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना, वेब सर्वर से जानकारी मांगने की प्रक्रिया है। प्रोग्रामर ऐसा अक्सर डाटा प्राप्त करने, API के साथ इंटरैक्ट करने, या सेवाओं को ऑटोमेट करने के लिए करते हैं।

## How to: (कैसे करें:)
Fish Shell में एक HTTP GET अनुरोध करने के लिए हम `curl` कमांड का इस्तेमाल करेंगे। यहां एक उदाहरण है:

```Fish Shell
curl http://example.com/api/data
```

इसे चलाने पर आपको कुछ इस तरह का आउटपुट दिखेगा:

```
{"status": "success", "data": [...]}
```

POST अनुरोध के लिए हम `-d` फ्लैग का इस्तेमाल कर सकते हैं:

```Fish Shell
curl -d "param1=value1&param2=value2" http://example.com/api/submit
```

आउटपुट में आपको अनुरोध की स्थिति की जानकारी मिलेगी।

## Deep Dive (गहराई से जानकारी)
HTTP अनुरोध भेजना, वेब की बुनियादी बातों में से एक है। 1990 के दशक से, HTTP प्रोटोकॉल वेब पर संचार का मुख्य ढांचा है। अनुरोध भेजने के विकल्पों में `wget`, `httpie` या फिश शेल के बिल्ट-इन फंक्शन शामिल हैं। `curl` अपने फ्लेक्सिबिलिटी और व्यापक प्लेटफॉर्म सपोर्ट के कारण अधिक प्रचलित है। इसे इस्तेमाल करते समय, -X फ्लैग के साथ हम HTTP की विभिन्न मेथड जैसे GET, POST, PUT, DELETE आदि का स्पेसिफिकेशन कर सकते हैं।

## See Also (और देखें)
1. `curl` के बारे में और जानें: https://curl.haxx.se/
2. फिश शेल डॉक्यूमेंटेशन: https://fishshell.com/docs/current/index.html
3. `httpie` एक और सरल HTTP क्लाइंट: https://httpie.io/
