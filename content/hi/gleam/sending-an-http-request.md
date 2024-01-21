---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:55.316934-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना मतलब वेब सर्वर से जानकारी मांगना या भेजना। प्रोग्रामर्स इसे डाटा का आदान-प्रदान करने के लिए करते हैं।

## How to: (कैसे करें:)
Gleam में HTTP अनुरोध भेजने के लिए `gleam/http` का उपयोग करते हैं।

```gleam
import gleam/http
import gleam/should

pub fn main() {
  let response = http.get("https://jsonplaceholder.typicode.com/posts/1")
  should.equal(response.status, 200)
  should.equal(response.body, Some("{ ... JSON DATA ... }"))
}
```

ऊपर दिया गया कोड सामान्यतः एक HTTP GET अनुरोध भेजता है और स्टेटस कोड 200 (सफलता) के साथ जवाब की उम्मीद करता है।

## Deep Dive (गहराई से जानकारी)
HTTP अनुरोध इंटरनेट की नींव हैं, जो 1990 के दशक से वेब कम्युनिकेशन में प्रयोग किए जा रहे हैं। Gleam में, `gleam/http` एक बेहतरीन HTTP क्लाइंट प्रदान करता है, जो Erlang के `httpc` module के ऊपर बनाया गया है। ग्लीम में अन्य भाषाओं की तुलना में एक बहुत सरल और टाइप-सेफ तरीका होता है HTTP अनुरोध करने का।

Gleam आपको कम्पाइल-टाइम पर ही कई त्रुटियों से बचाता है, फिर भी रनटाइम पर परफॉर्मेंस भी अच्छी देता है। वैकल्पिक रूप से, आप `gleam/http` के साथ `gleam/reqwest` या `gleam/surf` जैसे क्रेट्स का इस्तेमाल करके अधिक उन्नत कार्यक्षमता जोड़ सकते हैं।

## See Also (और देखें)
- Gleam हाउटू HTTP: [Gleam HTTP howto](https://hexdocs.pm/gleam_http)
- Gleam की आधिकारिक वेबसाइट: [Gleam's Official Website](https://gleam.run)
- HTTP स्पेसिफिकेशन: [HTTP specification](https://www.ietf.org/rfc/rfc2616.txt)