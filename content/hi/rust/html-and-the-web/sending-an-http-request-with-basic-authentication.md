---
date: 2024-01-20 18:03:10.755321-07:00
description: "HTTP \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u0935\u0947\
  \u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0907\u0928\u094D\u092B\u0949\
  \u0930\u094D\u092E\u0947\u0936\u0928 \u092E\u093E\u0902\u0917\u0928\u0947 \u0915\
  \u093E \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092C\u0947\
  \u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u092E \u0924\u092C \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 \u091C\u092C \u0939\u092E\u0947\u0902 \u0909\u0938\
  \ \u0907\u0928\u094D\u092B\u0949\u0930\u094D\u092E\u0947\u0936\u0928 \u0924\u0915\
  \ \u092A\u0939\u0941\u0902\u091A\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092F\
  \u0942\u091C\u0930\u0928\u0947\u092E\u2026"
lastmod: '2024-03-13T22:44:51.961191-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u0935\u0947\
  \u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0907\u0928\u094D\u092B\u0949\
  \u0930\u094D\u092E\u0947\u0936\u0928 \u092E\u093E\u0902\u0917\u0928\u0947 \u0915\
  \u093E \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092C\u0947\
  \u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u092E \u0924\u092C \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 \u091C\u092C \u0939\u092E\u0947\u0902 \u0909\u0938\
  \ \u0907\u0928\u094D\u092B\u0949\u0930\u094D\u092E\u0947\u0936\u0928 \u0924\u0915\
  \ \u092A\u0939\u0941\u0902\u091A\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092F\
  \u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930 \u092A\u093E\u0938\u0935\u0930\
  \u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u0935\u0947\u0930\u093F\u092B\u093E\
  \u0908 \u0915\u0930\u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें? (How to:)
Rust में `reqwest` क्रेट का इस्तेमाल करके बेसिक ऑथेंटिकेशन के साथ HTTP रिक्वेस्ट भेजना सीखते हैं:

```Rust
// आपके Cargo.toml में निम्न dependency जोड़ें
// [dependencies]
// reqwest = "0.11"
// tokio = { version = "1", features = ["full"] }

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let client = reqwest::Client::new();
    let user = "user123";
    let pass = "password456";

    let response = client.get("http://example.com")
        .basic_auth(user, Some(pass))
        .send()
        .await?;

    println!("Status: {}", response.status());
    println!("Headers:\n{:?}", response.headers());

    Ok(())
}
```

जब आप यह कोड चलाते हैं, आपको रिस्पॉन्स मिलेगा जैसे:

```
Status: 200 OK
Headers:
{
    ...
}
```

## गहराई से जानकारी (Deep Dive)
HTTP बेसिक ऑथेंटिकेशन एक सिंपल लेकिन कमजोर सिक्योरिटी प्रोटोकॉल है। यह बेस64 इन्कोडिंग का इस्तेमाल करता है, जो कि एन्क्रिप्शन नहीं है। इसीलिए इसका इस्तेमाल आमतौर पर HTTPS के साथ किया जाता है।

वैकल्पिक तरीके हो सकते हैं जैसे कि ओआईडीसी (OIDC), ओअथ2 (OAuth2) या मल्टी-फैक्टर ऑथेंटिकेशन, जो अधिक सुरक्षित होते हैं।

बेसिक ऑथेंटिकेशन का इस्तेमाल जब भी आप `basic_auth` मेथड लगाते हो, हेडर में `Authorization` जोड़े जाते हैं जिसमें `username:password` बेस64-इन्कोडेड स्ट्रिंग के रूप में होते हैं।

## और भी जानकारी (See Also)
- Rust `reqwest` क्रेट का डॉक्युमेंटेशन: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- बेसिक ऑथेंटिकेशन स्पेसिफिकेशन: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Rust async नेटवर्किंग के लिए Tokio ट्यूटोरियल: [https://tokio.rs/tokio/tutorial](https://tokio.rs/tokio/tutorial)
