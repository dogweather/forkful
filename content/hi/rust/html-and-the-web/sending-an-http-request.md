---
title:                "HTTP अनुरोध भेजना"
aliases:
- /hi/rust/sending-an-http-request/
date:                  2024-01-20T18:21:45.486699-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request.md"
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
