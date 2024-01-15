---
title:                "यामल के साथ काम करना"
html_title:           "Go: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

YAML, यानि "YAML Ain't Markup Language", एक नए स्ट्रक्चर कोडिंग लैंग्वेज है जो डेटा संरचना को सरल बनाने के लिए डिज़ाइन किया गया है। यह आसान समझने और उपयोग करने के लिए है और इसलिए वे जो लोग प्रोग्रामिंग से नए होते हैं, वे YAML के साथ प्रवेश द्वार खोल सकते हैं।

## कैसे करें

आप अपने Go प्रोजेक्ट में YAML फ़ाइल बनाने के लिए `go get` कमांड का इस्तेमाल कर सकते हैं। YAML को लोड करने के लिए आपको `gopkg.in/yaml.v2` को इम्पोर्ट करना होता है। यहां आपको YAML फ़ाइल से डेटा पढ़ने और लिखने के लिए कुछ उदाहरण ऐसे हैं:

```go
package main

import (
    "fmt"
    "io/ioutil"

    "gopkg.in/yaml.v2"
)

func main() {
    // YAML फ़ाइल से डेटा पढ़ें
    data, err := ioutil.ReadFile("config.yaml")
    if err != nil {
        panic(err)
    }

    // मानों को डेटा स्ट्रक्चर में डेकोड करें
    config := make(map[string]string)
    err = yaml.Unmarshal(data, &config)
    if err != nil {
        panic(err)
    }

    // मानों को डेटा स्ट्रक्चर से एक्सेस करें
    fmt.Println(config["name"])

    // YAML फ़ाइल में नए मानों को लिखें
    config["company"] = "Acme Corporation"
    // लिखें YAML फ़ाइल
    data, err = yaml.Marshal(config)
    if err != nil {
        panic(err)
    }
    err = ioutil.WriteFile("config.yaml", data, 0644)
    if err != nil {
        panic(err)
    }
}
```

आप अपने प्रोजेक्ट में ये कोड एड करके YAML से आसानी से डेटा पढ़ सकते हैं और उसमें नए मान लिख सकते हैं।

## गहराई में जाएँ

YAML प्रोग्रामिंग में बहुत ही उपयोगी है क्योंकि यह डेटा संरचना को दिखाने के लिए बहुत ही संवेदनशील है। आप इसमें कमेंट्स, माल्टीलाइन स्ट्रिंग