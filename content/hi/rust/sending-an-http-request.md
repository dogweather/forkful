---
title:                "Rust: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## हम क्यों?

अगर आप एक भाषा में लिखे गए कंप्यूटरीकृत प्रोग्राम को दूसरी भाषा में बताना चाहते हैं, तो आपको HTTP अनुरोध भेजने की जरूरत होती है। एक HTTP अनुरोध क्या है यह आपको भाषांतर और संपर्क दोनों को कम दिखाता है। यह आपको अपने प्रोग्राम को अन्य आकड़ों से भी संपर्क करने की अनुमति देता है।

## कैसे करें?

आइए हम एक उदाहरण को देखें जिसमें हम एक HTTP अनुरोध भेजेंगे। इस के लिए, हमें हायपर पुस्तकालय का उपयोग करना होगा जो हमें HTTP से संबद्ध कुछ नमुनाएं प्रदान करता है। नीचे दिए गए उदाहरण में, हम URL को बनाते हैं और उसे एक HTTP अनुरोध के साथ जोड़ते हैं। हम फिर रिक्वेस्‍ट को भेजते हैं और उसकी प्रतिक्रिया को प्रिंट करते हैं।

```Rust 
use hyper::{Body, Client, Uri};

async fn send_request() {
    // URL बनाएं
    let url = Uri::from_static("https://jsonplaceholder.typicode.com/posts/1");

    // CLIENT बनाएं
    let client = Client::new();

    // अनुरोध भेजें
    let response = client.get(url).await.unwrap();

    // प्रिंट उत्तर
    println!("response: {:?}", response); 
}
```

जब हम इस कोड को चलाते हैं, हमें निम्नलिखित आउटपुट मिलता है:
`response: Response { status: 200, data: User-Agent: hyper/0.14.8, content-length: 2 }`

हम देख सकते हैं कि हमारा अनुरोध सफल है और हमें एक 200 स्थिति कोड मिला है।

## गहराई में जाएं

HTTP अनुरोध भेजना बहुत अध्ययनसार और रूस्त की दुनिया में एक महत्वपूर्ण टॉपिक है। एचटीएमएल के माध्यम से एक नेटवर्क स्त