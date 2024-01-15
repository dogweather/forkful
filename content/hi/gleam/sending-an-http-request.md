---
title:                "एक http अनुरोध भेजना"
html_title:           "Gleam: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# क्यों

इंटरनेट पर, लगभग सभी वेब ऐप्स वेब सर्वर के साथ कम से कम एक HTTP रिक्वेस्ट भेजते हैं। यह रिक्वेस्ट वेब सर्वर को डेटा, चित्र, अथवा अन्य सामग्री का अनुरोध करता है। आईए कुछ हिंदी में घेट में अवशोषित हो!

# कैसे

```Gleam
import gleam/http

baseUrl = "https://www.example.com"
path = "/api/items"

url = baseUrl ++ path

response = http.get(url)

let items = Json.Decode.decodeString(response.body)

items
```

इस कोड द्वारा, हम दिया गया "बेस URL" और "पथ" का HTTP स्ट्रिंग स्पन करते हैं और वेब सर्वर से डेटा को प्राप्त करते हैं। इस उदाहरण में, हम एक "थिंग" वेब सर्वर से प्राप्त करते हैं जो Json फाइल में अपशिष्ट है। अपशिष्ट डेवोपमेंट और उसके साथ नियोजित उदाहरण में बढ़ती सुविधा के कारण, 3s का निर्देश प्राप्त होता है की वेब सर्वर्स के साथ धारा हुई धारा: एमएलके.

# गहराई में

अगर आप पूरी तरह से गहरी अवगनिती करना चाहते हैं, तो आप उपयोग कर सकते हैं। यह इनPut औपट सित्रोल में शामिल है। आईए इसी एक्स्टेन्सिओं द्वारा उत्पन्न जा सकता है।

# आगे देखें

अधिक जानकारी के लिए, इन लिंक्स को देखें: 

- [Gleam ऑफिशल वेबसाइट](https://gleam.run/)
- [HTTP आवेदगमन हस्तलिखित गाइड](https://gleam.run/articles/http-client/)