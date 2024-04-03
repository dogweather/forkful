---
date: 2024-01-20 18:21:45.486699-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Rust \u092E\
  \u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `reqwest` \u0915\u094D\u0930\u0947\u091F \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0964."
lastmod: '2024-03-13T22:44:51.956193-06:00'
model: gpt-4-1106-preview
summary: "Rust \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `reqwest` \u0915\u094D\u0930\
  \u0947\u091F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\
  \u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

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
