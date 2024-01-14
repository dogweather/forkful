---
title:                "Go: बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों

एचटीटीपी अनुरोध भेजने में बेसिक प्रमाणीकरण के साथ क्यों जुड़ना हो सकता है, इसके बारे में कुछ जानने के लिए कफी महत्वपूर्ण है।

# कैसे

अगर आप गो प्रोग्रामिंग सीख रहे हैं और एचटीटीपी अनुरोध भेजने में बेसिक प्रमाणीकरण का उपयोग करने के बारे में जानना चाहते हैं, तो आप सही जगह पर हैं। इस लेख में, हम गो कोड का उदाहरण देकर और उसके आउटपुट को दिखाकर आपको इस प्रक्रिया को अधिक समझने में मदद करेंगे।

```Go
package main

import (
    "fmt"
    "net/http"
    "time"
)

func main() {
    client := &http.Client{
        Timeout: time.Second * 10,
    }

    req, err := http.NewRequest("GET", "https://example.com", nil)
    if err != nil {
        log.Fatal("Error creating request:", err)
    }

    req.SetBasicAuth("username", "password")
    resp, err := client.Do(req)
    if err != nil {
        log.Fatal("Error sending request:", err)
    }

    fmt.Println(resp.StatusCode)
}
```

इस कोड में, हमने `http.NewRequest` फंक्शन का उपयोग करके नया एचटीटीपी अनुरोध बनाया है और `req.SetBasicAuth` के माध्यम से उपयोगकर्ता नाम और पासवर्ड को सेट किया है। फिर हमने `client.Do` के माध्यम से यह अनुरोध भेजकर उत्तर को लेकर उसके स्थिति को प्रिंट किया है। ऐसा करके, हम एचटीटीपी अनुरोध को बेसिक प्रमाणीकरण के साथ सफलतापूर्वक भेज सकते हैं।

# गहराई में

बेसिक प्रमाणीकरण HTTP अनुरोध भेजने के बारे में अधिक जानने के लिए, आपको इसका निरंतर उपयोग करना पड़ेगा। आप अपने गो कोड में `net/http` पैकेज और उसके `Client` और `Request` संरचनाओं के साथ खेल करके खु