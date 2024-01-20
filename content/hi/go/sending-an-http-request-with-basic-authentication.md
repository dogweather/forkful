---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP request भेजना और Basic Authentication से क्या मतलब होता है? यदि आप client-server architecture में काम कर रहे हो तो आपको इसका पता होना चाहिए। यह ऐसा तरीका है जिससे क्लाइंट और सर्वर के बीच सुरक्षित कनेक्शन स्थापित किया जा सकता है। इसकी मदद से, वेबसाइट्स या API क्लाइंट को उनकी गोपनीयता और सुरक्षा की गारंटी देते हैं। 

## कैसे करें:
Go लैंग्वेज में HTTP request भेजने का धादारन नीचे दिया गया है:

```Go
package main

import (
  "net/http"
  "fmt"
)

func main() {
  req, err := http.NewRequest("GET", "<url here>", nil)
  req.SetBasicAuth("<username>", "<password>")

  client := &http.Client{}
  resp, err := client.Do(req)
  
  if err != nil {
    fmt.Println(err)
  } else {
    fmt.Println(resp.Status)
  }
}
```
इसमें `<url here>` के स्थान पर आपको URL देना है, जिस पर request भेजनी है और `<username>` और `<password>` के स्थान पर authentication details देनी है।

## गहरी चर्चा:
HTTP Basic Authentication का उपयोग प्राय: APIs में किया जाता है, जहां आवश्यक सुरक्षा के लिए request header में username और password शामिल होते हैं। यह प्राणी 1990 के दशक से HTTP मानक का हिस्सा रहा है। वैकल्पिकरूप से, आप HTTPS का भी उपयोग कर सकते हैं, जो पूरी तरह सुरक्षित है। कृपया ध्यान दें कि Basic Authentication सभी request headers को बेस64 encoding में भेजता है, जो अगर टूट गई तो यूजरनेम और पासवर्ड को प्रकट कर सकती है।

## यह भी देखें:
- [HTTP Basic Authentication(@wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Go net/http package](https://pkg.go.dev/net/http)
- [The Go Programming Language Specification}(https://golang.org/ref/spec)