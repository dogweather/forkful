---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:02:22.843470-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध के साथ बुनियादी प्रमाणीकरण (Basic Authentication) एक प्रक्रिया है जिसमें सर्वर को उपयोगकर्ता का नाम और पासवर्ड भेजा जाता है ताकि सेवा प्रदान कर सके। प्रोग्रामर्स इसे तब करते हैं जब वे एक सुरक्षित API से जानकारी प्राप्त करना चाहते हैं या किसी सुरक्षित संसाधन तक पहुँचना चाहते हैं।

## कैसे करें: 

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"net/http"
)

func main() {
	url := "http://example.com/data"
	username := "मेरानाम"
	password := "मेरापासवर्ड"

	client := &http.Client{}
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		fmt.Println(err)
		return
	}

	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic "+auth)

	resp, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer resp.Body.Close()
	fmt.Println("HTTP Status Code:", resp.StatusCode)
	if resp.StatusCode == http.StatusOK {
		fmt.Println("सफलतापूर्वक प्रमाणीकृत!")
	} else {
		fmt.Println("प्रमाणीकरण असफल।")
	}
}
```
सैंपल आउटपुट:
```
HTTP Status Code: 200
सफलतापूर्वक प्रमाणीकृत!
```

## गहराई से समझे:

HTTP Basic Authentication एक पारंपरिक तकनीक है जो RFC 7617 में वर्णित है। इसमें username और password को Base64 में एनकोड कर एक HTTP हेडर में भेजा जाता है। 

हालाँकि, क्योंकि Base64 एनकोडिंग एक साधारण एनकोडिंग है जिसे आसानी से डिकोड किया जा सकता है, इसलिए HTTPS का इस्तेमाल किए बिना Basic Authentication असुरक्षित है। 

विकल्प के रूप में, वेब डेवलपर्स अक्सर OAuth, API keys या JWT (JSON Web Tokens) का इस्तेमाल करते हैं, जो अधिक सुरक्षित और लचीले होते हैं।

गो के `net/http` पैकेज का इस्तेमाल करते हुए यह और भी आसान हो जाता है। `NewRequest` फंक्शन के साथ HTTP अनुरोध बनाया जा सकता है और `Authorization` हेडर को `Add` मेथड से जोड़ा जा सकता है। 

## संबंधित स्रोत:

- Go डॉक्स पर net/http पैकेज: https://golang.org/pkg/net/http/
- RFC 7617, HTTP Basic Authentication: https://tools.ietf.org/html/rfc7617
- Go ब्लॉग पर JSON और Go: https://blog.golang.org/json

इस लेख में दिए गए उदाहरण और जानकारी आपको HTTP Basic Authentication की बुनियादी समझ प्रदान करते हैं और गो प्रोग्रामिंग भाषा में इसका क्रियान्वयन करने में मदद कर सकते हैं।