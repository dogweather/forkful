---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:07.105933-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Go \u092E\u0947\u0902\
  \ TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0936\u0941\u0930\u0942\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A\u0915\u094B\
  \ \u092A\u0939\u0932\u0947 \u090F\u0915 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0928\u0947 \u0915\u0940\
  \ \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\
  \u0948 \u091C\u094B TOML \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0930 \u0938\u0915\u0947, \u0915\u094D\u092F\u094B\
  \u0902\u0915\u093F Go\u2026"
lastmod: '2024-03-13T22:44:51.476812-06:00'
model: gpt-4-0125-preview
summary: "Go \u092E\u0947\u0902 TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  , \u0906\u092A\u0915\u094B \u092A\u0939\u0932\u0947 \u090F\u0915 \u0932\u093E\u0907\
  \u092C\u094D\u0930\u0947\u0930\u0940 \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\
  \u0928\u0947 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\
  \u094B\u0924\u0940 \u0939\u0948 \u091C\u094B TOML \u092B\u093E\u0907\u0932\u094B\
  \u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930 \u0938\u0915\u0947\
  , \u0915\u094D\u092F\u094B\u0902\u0915\u093F Go \u0938\u094D\u091F\u0948\u0902\u0921\
  \u0930\u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0938\
  \u094D\u0935\u093E\u092D\u093E\u0935\u093F\u0915 \u0930\u0942\u092A \u0938\u0947\
  \ TOML \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902\
  \ \u0915\u0930\u0924\u0940 \u0939\u0948\u0964 `BurntSushi/toml` \u092A\u0948\u0915\
  \u0947\u091C \u0907\u0938\u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0932\u094B\
  \u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\
  \u0948\u0964 \u092A\u0939\u0932\u0947, \u0907\u0938\u0947 \u0907\u0902\u0938\u094D\
  \u091F\u0949\u0932 \u0915\u0930\u0928\u093E \u0938\u0941\u0928\u093F\u0936\u094D\
  \u091A\u093F\u0924 \u0915\u0930\u0947\u0902."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

## कैसे करें:
Go में TOML के साथ काम शुरू करने के लिए, आपको पहले एक लाइब्रेरी शामिल करने की आवश्यकता होती है जो TOML फाइलों को पार्स कर सके, क्योंकि Go स्टैंडर्ड लाइब्रेरी स्वाभाविक रूप से TOML का समर्थन नहीं करती है। `BurntSushi/toml` पैकेज इसके लिए एक लोकप्रिय विकल्प है। पहले, इसे इंस्टॉल करना सुनिश्चित करें:

```bash
go get github.com/BurntSushi/toml
```

इसका उपयोग कैसे करें इसका एक सरल उदाहरण यहां दिया गया है। मान लीजिए आपके पास `config.toml` नामक एक कॉन्फ़िगरेशन फ़ाइल है जिसमें निम्नलिखित सामग्री होती है:

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

अब, आपको एक Go संरचना बनानी होगी जो TOML संरचना का प्रतिबिंब हो:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

नमूना आउटपुट:

```
Title: TOML Example
Database Server: 192.168.1.1
```

## गहराई में जानकारी
TOML को टॉम प्रेस्टन-वर्नर द्वारा बनाया गया था, जो GitHub के सह-संस्थापकों में से एक है, जिसे एक सरल कॉन्फ़िगरेशन फ़ाइल प्रारूप प्रदान करने के लिए बनाया गया था जिसे आसानी से हैश टेबल में मैप किया जा सकता है और बिना पहले से प्रारूप की जानकारी के एक नज़र में समझा जा सकता है। यह JSON या YAML के विपरीत है, जो कि, जबकि व्यापक रूप से उपयोग किया जाता है, कोष्ठक, उद्धरण, और इंडेंटेशन की समस्याओं के कारण कॉन्फ़िगरेशन फ़ाइलों के लिए कम मानव-अनुकूल हो सकता है।

Go में `BurntSushi/toml` पैकेज एक मजबूत लाइब्रेरी है जो केवल डिकोडिंग ही नहीं बल्कि TOML फाइलों की एन्कोडिंग भी करने की अनुमति देती है, जिससे यह उन अनुप्रयोगों के लिए एक बहुमुखी विकल्प बन जाती है जिन्हें इस प्रारूप में कॉन्फ़िगरेशन फ़ाइलें पढ़ने और लिखने की आवश्यकता होती है। हालांकि, एक को यह नोट करना चाहिए कि तकनीकों की उन्नति और नवीन Go संस्करणों के परिचय के साथ, `pelletier/go-toml` जैसे विकल्प सामने आए हैं, जो सुधारित प्रदर्शन और वृक्ष मैनिपुलेशन और क्वेरी समर्थन जैसे अतिरिक्त सुविधाओं की पेशकश करते हैं।

जबकि TOML कई अनुप्रयोगों के लिए एक शानदार विकल्प है, एप्लिकेशन कॉन्फ़िगरेशन की जटिलता और व्यक्तिगत या टीम प्राथमिकताओं के आधार पर, YAML या JSON जैसे अन्य प्रारूप बेहतर उपयुक्त हो सकते हैं, खासकर अगर कॉन्फ़िगरेशन को अधिक जटिल डेटा संरचनाओं की आवश्यकता होती है जो TOML की विस्तृत प्रकृति से सुरुचिपूर्वक पकड़ी नहीं जा सकती। फिर भी, सीधे, पढ़ने योग्य, और आसानी से संपादनीय कॉन्फ़िगरेशनों के लिए, TOML, Go के मजबूत प्रकार प्रणाली और उल्लेखित पुस्तकालयों के साथ, एक उत्कृष्ट विकल्प है।
