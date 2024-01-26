---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:05.595183-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध भेजना यानी सर्वर से डेटा मंगाना या सर्वर को डेटा भेजना। प्रोग्रामर इसे वेब सेवाओं से जुड़ने, डेटा साझा करने, API कॉल करने के लिए करते हैं।

## How to: (कैसे करें:)

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	// GET अनुरोध
	resp, err := http.Get("https://jsonplaceholder.typicode.com/posts/1")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(body))
}
```

सैंपल आउटपुट:
```
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita..."
}
```

## Deep Dive (गहन जानकारी)

HTTP अनुरोध भेजने की प्रक्रिया इंटरनेट के प्रारंभ से ही है। `net/http` पैकेज Go में इस काम के लिए आदर्श माना जाता है। इसमें GET, POST, PUT, DELETE जैसे विभिन्न प्रकार के अनुरोध भेजे जा सकते हैं। `net/http` के अलावा अन्य पुस्तकालय जैसे `gorilla/mux` या `gin-gonic/gin` भी मौजूद हैं जिनका विशेष परिस्थितियों में उपयोग होता है। Go में HTTP क्लाइंट का उपयोग करते समय `defer resp.Body.Close()` का प्रयोग करना महत्वपूर्ण है ताकि रिसोर्स लीक न हो।

## See Also (और देखें)

- Go के डॉक्युमेंटेशन पर `net/http` पैकेज: [Go net/http](https://pkg.go.dev/net/http)
- Go में `http` क्लाइंट और सर्वर के उपयोग के लिए ट्यूटोरियल्स: [Go by Example](https://gobyexample.com/http-clients) और [Go by Example](https://gobyexample.com/http-servers)
- गहराई से जानकारी के लिए ब्लॉग पोस्ट: [The Go Blog](https://blog.golang.org/http-tracing)
