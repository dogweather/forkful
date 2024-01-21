---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:08.039561-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक वेब पेज को डाउनलोड करने का अर्थ है इंटरनेट से सूचना प्राप्त करना। प्रोग्रामर्स यह तब करते हैं जब उन्हें डाटा एकत्र करना होता है या किसी वेबसाइट से आटोमेटिक रूप से इंटरैक्ट करना होता है।

## How to: (कैसे करें:)
Gleam में, `httpc` मॉड्यूल का इस्तेमाल करके हम वेब पेज डाउनलोड कर सकते हैं:

```gleam
import gleam/http/httpc
import gleam/io

pub fn download_page() {
  case httpc.get("http://example.com") {
    Ok(response) -> io.println(response.body)
    Error(error) -> io.println("Failed to download page: ", error)
  }
}
```

जब आप इस फंक्शन को चलाएँगे, आपको http://example.com का HTML कंटेंट मिल जाएगा।

## Deep Dive (गहरी जानकारी)
वेब पेज डाउनलोड करना काफी कॉमन टास्क है, जिसे पुराने ज़माने से `HTTP GET` रिक्वेस्ट के जरिए किया जाता है। Gleam में `httpc` एक बिल्ट-इन मॉड्यूल है जिसे इसी उद्देश्य के लिए उपयोग किया जा सकता है। वैकल्पिक तरीके में `curl` या हाई-लेवल HTTP क्लाइंट लाइब्रेरीज शामिल हैं। इम्प्लीमेंटेशन विस्तार में जाने पर, आप `HTTP` प्रोटोकॉल, `TCP/IP` स्टैक, और डीप पार्सिंग ऑफ HTML जैसे मुद्दों का पता लगा सकते हैं।

## See Also (और देखें)
- [Mozilla Developer Network - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [HTML Parsing in Gleam](https://hexdocs.pm/gleam_html)