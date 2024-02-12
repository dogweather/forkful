---
title:                "HTTP अनुरोध भेजना"
aliases:
- /hi/go/sending-an-http-request.md
date:                  2024-02-03T18:09:43.216600-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTTP अनुरोध भेजना"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजने का तात्पर्य आपके Go एप्लिकेशन से वेब सर्वर, API, या किसी अन्य HTTP-आधारित सेवा को कॉल आरंभ करना होता है। प्रोग्रामर इस कार्य को वेब संसाधनों से जुड़ने, डेटा प्राप्त करने, फॉर्म जमा करने, या इंटरनेट भर में अन्य सेवाओं के साथ संवाद करने के लिए करते हैं।

## कैसे करें:

Go में, HTTP अनुरोध भेजने और उत्तर प्राप्त करने का कार्य `net/http` पैकेज का उपयोग करके किया जाता है। यहाँ एक साधारण GET अनुरोध भेजने और उत्तर पढ़ने का क्रमबद्ध उदाहरण दिखाया गया है:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // संसाधन के URL को परिभाषित करें
    url := "http://example.com"

    // GET अनुरोध भेजने के लिए http.Get का उपयोग करें
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // फ़ंक्शन के अंत में उत्तर बॉडी को बंद करें
    defer resp.Body.Close()

    // उत्तर बॉडी को पढ़ें
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // उत्तर बॉडी को स्ट्रिंग में बदलकर इसे प्रिंट करें
    fmt.Println(string(body))
}
```

नमूना आउटपुट (संक्षेप में):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

फ़ॉर्म डेटा के साथ POST अनुरोध भेजने के लिए, आप `http.PostForm` का उपयोग कर सकते हैं:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // URL और फ़ॉर्म डेटा को परिभाषित करें
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // फ़ॉर्म डेटा के साथ POST अनुरोध भेजें
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // उत्तर पढ़ें और प्रिंट करें
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## गहराई में:

Go में `net/http` पैकेज HTTP सर्वरों के साथ इंटरैक्ट करने का एक शक्तिशाली और लचीला तरीका प्रदान करता है। इसका डिज़ाइन Go के सरलता, कार्यक्षमता, और मजबूती पर जोर देने को दर्शाता है। मूल रूप से, JSON या XML पेलोड को हैंडल करने जैसी कार्यक्षमताएं मैन्युअली अनुरोध बॉडी बनाने और उचित हैडर्स सेट करने की आवश्यकता थी। जैसे-जैसे Go विकसित हुआ, समुदाय ने इन कार्यों को और अधिक सरल बनाने के लिए उच्च-स्तरीय पैकेज विकसित किए, जैसे कि रूटिंग के लिए `gorilla/mux` और JSON मैनिपुलेशन के लिए `gjson`।

Go के HTTP क्लाइंट की एक उल्लेखनीय विशेषता इसका इंटरफेस और स्ट्रक्ट्स, जैसे कि `http.Client` और `http.Request`, का उपयोग है, जो व्यापक अनुकूलन और परीक्षण की अनुमति देता है। उदाहरण के लिए, आप प्रदर्शन के लिए `http.Client` को अनुरोधों का समय समाप्त करने या कनेक्शन को जीवित रखने के लिए संशोधित कर सकते हैं।

सरल HTTP इंटरैक्शन के लिए एक विचारित विकल्प "Resty" या "Gentleman" जैसी तृतीय-पक्ष लाइब्रेरीज़ का उपयोग करना है। ये पैकेज एक उच्च-स्तरीय अमूर्तिकरण के लिए HTTP अनुरोध प्रदान करते हैं, सामान्य कार्यों को अधिक संक्षिप्त बनाते हैं। हालांकि, अधिक जटिल या अनोखे HTTP इंटरैक्शन परिदृश्यों से निपटने के लिए `net/http` पैकेज को समझना और उपयोग करना महत्वपूर्ण है, जो Go की साझादारी सुविधाओं और शक्तिशाली मानक पुस्तकालय का पूर्ण लाभ उठाने की आधारशिला प्रदान करता है।
