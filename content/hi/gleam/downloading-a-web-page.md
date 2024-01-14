---
title:                "Gleam: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

अब जब हम दूसरों के साथ चीजों को सांझा करने या जानने के लिए इंटरनेट का इस्तेमाल करते हैं, हम दूसरे वेब पृष्ठों को भी डाउनलोड कर सकते हैं। यह ग्राहक-सर्वर आधारित रीक्तता से संभव होता है, जिससे ताजा डाटा को आपके वेब पेज पर प्रदर्शित किया जा सकता है। इसलिए, वेब पृष्ठों को डाउनलोड करना बहुत महत्वपूर्ण है।

## कैसे करें

पहले हमें Gleam प्रोग्रामिंग भाषा को रन करने का एन्वायरनमेंट सेट अप करना होगा। अब हम दूसरे वेब पृष्ठों को डाउनलोड करने के लिए एक HTTP क्लाइंट कमांड लाइन प्रोग्राम लिखेंगे। सबसे पहले, ```Gleam_httpc``` लाइब्रेरी को जोड़ते हैं। फिर ```Gleam_httpc.get``` का उपयोग कर वेब पृष्ठ का URL पाता है और उसको डाउनलोड करता है। नीचे दिए गए उदाहरण में हम ```https://www.example.com``` को डाउनलोड करेंगे और इसका प्रिंट आउट दिखाएंगे।

```Gleam
use std/io

httpc = Gleam_httpc.new()

url = "https://www.example.com"

result = Gleam_httpc.get(httpc, url)

case result {
  Ok(http_response) -> {
    status_code = io.int_to_string(http_response.status_code)
    println("Status code: #{status_code}")
    
    headers = io.to_lower_map(http_response.headers)
    content_type = case io.map.get(headers, "content-type") {
      Some(value) -> value
      None -> "none"
    }
    println("Content type: #{content_type}")
    
    body = io.bytes_to_list(http_response.body)
    println("Body: #{body}")
  }
  
  Err(message) -> {
    println("Error: #{message}")
  }
}
```

आप ऊपर लिखे गए कोड को रन करके आप उदाहरण में दिखाए गए तीनों वस्तुओं को वापस प्राप्त कर सकते हैं। आप यहां से अपने कोड में परिवर्तन करके अन्य वेब पृष्ठों को डाउनलोड करने