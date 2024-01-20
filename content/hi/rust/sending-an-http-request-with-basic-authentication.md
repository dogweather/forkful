---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# एचटीटीपी अनुरोध के साथ बेसिक प्रमाणीकरण भेजना रस्ट (Rust) में 

## क्या और क्यों?
बेसिक प्रमाणीकरण (Basic Authentication) क्षेतु में, एचटीटीपी अनुरोध भेजना से आप वेब सेवाओं और एपीआई के साथ सुरक्षित बातचीत स्थापित करते हैं। यह सुनिश्चित करता है कि केवल प्राधिकृत उपयोगकर्ताओं को ही पहुंच हो। 

## कैसे करें:
```Rust
use reqwest::{Client, header};
use std::collections::HashMap;

pub async fn send_request() -> Result<(), reqwest::Error> {
   let mut headers = header::HeaderMap::new();
   let client = Client::new();
   
   let username = "user1";
   let password = "password123";
   
   headers.insert("Authorization", header::HeaderValue::from_str(&format!("Basic {}", base64::encode(&format!("{}:{}", username, password))))?);
   
   let res = client.get("https://httpbin.org/anything")
       .headers(headers)
       .send()
       .await?;
   
   println!("{:?}", res.status());
   
   Ok(())
}
```

यदि आपकी प्राधिकृति सही है, तो आपको 200 OK स्थिति मिलेगी। यदि आपकी प्राधिकृति अमान्य है, तो आपको 401 Unauthorized मिलेगा।

## गहराई में:
बेसिक प्रमाणीकरण, वेब प्रमाणीकरण की पुरानी तकनीक है। अपनी सरलता के कारण, इसे आज भी काफी मात्रा में इस्तेमाल किया जाता है, हालांकि इसे SSL / TLS के साथ संरक्षित कनेक्शन पर ही इस्तेमाल करने की सलाह दी जाती है। 

विकल्प स्वरूप, आप Bearer Token अथॉराइजेशन, OAuth, या डाइजेस्ट प्रमाणीकरण का उपयोग कर सकते हैं। 

Rust में, `reqwest` क्रेट (crate) HTTP अनुरोध को संभालने के लिए सबसे लोकप्रिय विकल्प है। इसे सिंकनेस (sync) और असिंकनेस (async) दोनों परंपराओं में कार्यान्वित किया जा सकता है। 

## देखें भी:
रस्ट (Rust) में Proceeding HTTP अनुरोध करने के बारे में और जानने के लिए, निम्नलिखित संसाधनों का अन्वेषण करें:

2. The [reqwest crate](https://docs.rs/reqwest/0.11.3/reqwest/) documentation.
3. [Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
4. [How to send HTTP Requests in Rust](https://www.section.io/engineering-education/rust-http-request/) guide.