---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Rust में HTTP अनुरोध भेजना: साइनसे के पिछे

## क्या और क्यों?

HTTP अनुरोध भेजना, इसका अर्थ होता है कि हम इंटरनेट पर एक सर्वर से जानकारी मांग रहे हैं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डाटा को खोज सकें और मशीन्स को संवादित कर सकें।

## कैसे करें:

Rust में, हम `reqwest` crate का उपयोग करके एक HTTP अनुरोध भेज सकते हैं। यहां एक उदाहरण है:

```Rust
use reqwest::Error;

async fn get_data() -> Result<(), Error> {
    let res = reqwest::get("https://httpbin.org/ip").await?;

    let body = res.text().await?;
    println!("बॉडी:\n{}", body);

    Ok(())
}
```

यह कोड सर्वर से डाटा के प्राप्त करने की कोशिश करता है और इसे कंसोल पर प्रिंट करता है। ऐसा करने पर, आपको निम्नलिखित आउटपुट मिलेगा:

```Rust
बॉडी:
{
  "origin": "58.87.32.100"
}
```

## गहरी गोता

HTTP अनुरोधों का प्रयोग 90 के दशक के मध्य से हो रहा है, जब वेब स्टार्ट हुआ था। अल्ट्रनेटिवली, Rust में `hyper` और `http` जैसे अन्य crates भी हैं जो HTTP अनुरोधों को सहयोग देते हैं, लेकिन `reqwest` क्रेट का सरलता और क्षमता का मिश्रण उन्हें एक अच्छा विकल्प बनाता है।

जब आप `reqwest::get` कॉल करते हैं, तो यह एक GET HTTP अनुरोध को विनिर्माण करता है और वो अनुरोध सर्वर के पास भेजता है।

## और देखें

अधिक जानकारी के लिए, Rust का आधिकारिक [reqwest crate documentation](https://docs.rs/reqwest) देखें। HTTP के इतिहास और विलेखन के बारे में जानने के लिए, [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP) की छाँव में पढ़ें।