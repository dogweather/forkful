---
title:                "Go: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने के लिए कोई कारण है कि क्यों यह जरूरी है।

## कैसे करें

एचटीटीपी अनुरोध भेजने के लिए आपको कुछ स्टेप्स का पालन करना होगा। आप निम्नलिखित उदाहरण और कोड ब्लॉक के साथ इसे कैसे किया जाता है इसके बारे में अधिक जान सकते हैं।

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    // एक नया एचटीटीपी अनुरोध बनाएं
    req, err := http.NewRequest("GET", "https://www.example.com", nil)
    if err != nil {
        panic(err)
    }

    // अनुरोध भेजें
    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // अनुरोध का प्रिंट करें
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    fmt.Println(string(body))
}
```

उपरोक्त कोड का उपयोग करके आप किसी भी वेबसाइट से एचटीटीपी रिक्वेस्ट भेज सकते हैं और उसका रिस्पॉन्स प्राप्त कर सकते हैं। इसमें आपको एक नया एचटीटीपी अनुरोध बनाना, उसे भेजना और रिस्पॉन्स का उपयोग करना सीखने को मिलेगा।

## डीप डाइव

एचटीटीपी अनुरोध भेजने का अधिक गहराई में जानने के लिए, आप इसके अंतर्निहित कार्यों को समझ सकते हैं। इसमें आपको एचटीटीपी के अनुसार कैसे रिक्वेस्ट, रिस्पॉन्स और क्लाइंट बनाएं तथा कैसे उनका उपयोग करें, उनके साथ कैसे काम करें और अन्य उपयोगी जानकारी शामिल हो सकती है। बहुत से विकल्प हैं जो आपको एचटीटीपी अनुरोध को अपने आव