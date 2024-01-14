---
title:                "Rust: बेसिक प्रमाणवाद के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणवाद के साथ एक http अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध भेजने के लिए बुनियादी प्रमाणीकरण के साथ काम करने का *कारण* यह हो सकता है कि आपको किसी विशिष्ट वेब साइट से डेटा अपनी एप्लिकेशन में लोड करना हो, जो कि बुनियादी प्रमाणीकरण को आवश्यकता हो सकती है।

## कैसे करें

अब कि हम जानते हैं कि *क्यों* हमें एक HTTP अनुरोध भेजने की आवश्यकता हो सकती है, हम इसको कैसे करें इस पर चर्चा करेंगे। पूर्वावलोकन में, हम एक दूसरी वेब साइट से डेटा का लोड बनाने के लिए एक HTTP GET अनुरोध भेजेंगे जिसमें बुनियादी प्रमाणीकरण का उपयोग करेंगे।

```Rust

use reqwest::Client;
use reqwest::header::HeaderValue;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {

    // URL डालें जहाँ से आप डेटा लोड करना चाहते हैं
    let url = "https://example.com";

    // अब थापने के हेडर्स डालेंगे 
    let mut headers = header::HeaderMap::new();
    
    // ओथराइज़ेशन् हेडर डालें
    headers.insert("Authorization", HeaderValue::from_static("Basic abc123"));

    // Reqwest क्लाइंट बनाएं 
    let client = Client::new();   

    // अनुरोध भेजें
    let response = client
        .get(url)
        .headers(headers)
        .send()
        .await?;
    
    println!("{:#?}", response.text().await?);

    Ok(())
}

```

### उत्पाद

```
"<!DOCTYPE html><html><head><title>Example</title>...</head>...</body></html>"
```

## गहराई में जाएं

यह लेख HTTP अनुरोध भेजने के बुनियादी प्रमाणीकरण को संबंध में गहराई से समझने में मदद करेगा। आपने देखा होगा कि हमने `Authorization` हेडर कैसे जोड़ा है और उसे जैसा प्रमाणीकरण दिया है। आप इसका उपयोग अन्य प्रकार के HTTP अनुरोधों को भेजने के लिए भी कर सकते हैं। आप अ