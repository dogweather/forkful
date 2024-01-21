---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:03:10.755321-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP रिक्वेस्ट वेब सर्वर से इन्फॉर्मेशन मांगने का एक तरीका है। बेसिक ऑथेंटिकेशन का उपयोग हम तब करते हैं जब हमें उस इन्फॉर्मेशन तक पहुंचने के लिए यूजरनेम और पासवर्ड के साथ वेरिफाई करना होता है।

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