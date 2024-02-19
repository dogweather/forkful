---
aliases:
- /hi/rust/sending-an-http-request/
date: 2024-01-20 18:21:45.486699-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0921\u0947\u091F\u093E \u0932\u0947\
  \u0928\u0947 \u092F\u093E \u092D\u0947\u091C\u0928\u0947 \u0915\u093E \u090F\u0915\
  \ \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 API \u0915\u0949\u0932\
  \u094D\u0938 \u0915\u0930\u0928\u0947, \u0935\u0947\u092C\u0938\u093E\u0907\u091F\
  \u094D\u0938 \u0915\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092A\u094D\
  \u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947, \u092F\u093E \u0930\u093F\
  \u092E\u094B\u091F \u0938\u0930\u094D\u0935\u0930\u094D\u0938 \u0938\u0947\u2026"
lastmod: 2024-02-18 23:09:02.950429
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0921\u0947\u091F\u093E \u0932\u0947\
  \u0928\u0947 \u092F\u093E \u092D\u0947\u091C\u0928\u0947 \u0915\u093E \u090F\u0915\
  \ \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 API \u0915\u0949\u0932\
  \u094D\u0938 \u0915\u0930\u0928\u0947, \u0935\u0947\u092C\u0938\u093E\u0907\u091F\
  \u094D\u0938 \u0915\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092A\u094D\
  \u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947, \u092F\u093E \u0930\u093F\
  \u092E\u094B\u091F \u0938\u0930\u094D\u0935\u0930\u094D\u0938 \u0938\u0947\u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना सर्वर से डेटा लेने या भेजने का एक तरीका है। प्रोग्रामर्स इसे API कॉल्स करने, वेबसाइट्स की जानकारी प्राप्त करने, या रिमोट सर्वर्स से संवाद स्थापित करने के लिए करते हैं।

## How to: (कैसे करें:)
Rust में HTTP अनुरोध भेजने के लिए `reqwest` क्रेट का उपयोग करें।

```rust
use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let resp = reqwest::get("https://api.github.com/users/github").await?;
    println!("Status: {}", resp.status());
    let body = resp.text().await?;
    println!("Body:\n{}", body);
    Ok(())
}
```

सैम्पल आउटपुट:

```
Status: 200 OK
Body:
{
  "login": "github",
  ...
}
```

## Deep Dive (गहराई में):
HTTP अनुरोध इंटरनेट के आरंभ से ही वेब कम्युनिकेशन का आधार रहे हैं। Rust में `reqwest` लोकप्रिय है, पर `hyper` और `curl` जैसे क्रेट्स भी उपलब्ध हैं। `reqwest` वैसे तो `hyper` पर आधारित है, लेकिन यह अधिक उच्च-स्तरीय एबीआई प्रदान करता है। Rust में एसिंक्रोनस कोड हैंडलिंग के लिए `tokio` या `async-std` रनटाइम की आवश्यकता होती है, जो फाइबर/ग्रीन थ्रेड्स के बिना एफिशिएंट कोड का प्रबंधन करते हैं।

## See Also (और देखें):
- [Reqwest Crate Documentation](https://docs.rs/reqwest/)
- [Hyper Crate Documentation](https://docs.rs/hyper/)
- [Curl Crate Documentation](https://docs.rs/curl/)
- [Async Programming in Rust with async-std](https://async.rs/)
- [The Rust Asynchronous Book](https://rust-lang.github.io/async-book/)
