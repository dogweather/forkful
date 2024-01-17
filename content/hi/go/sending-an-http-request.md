---
title:                "एक http अनुरोध भेजना"
html_title:           "Go: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक HTTP अनुरोध भेजना मतलब दूसरे कंप्यूटर से डेटा को मांगना। यह डेटा अपने एप्लिकेशन में प्रयोग करने के लिए स्टोर किया जाता है और यह दूसरी वेबसाइटों या सेवाओं से संचार करने के लिए भी किया जाता है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे सुविधाजनक और स्केलेबल ऐप्लिकेशन बनाने में मदद मिलती है।

## कैसे करें:
```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {

	// HTTP GET request
	resp, err := http.Get("https://example.com")

	// Handle error
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	// Print response status code
	fmt.Println("Status Code:", resp.Status)

	// Close response body to prevent memory leaks
	defer resp.Body.Close()

}
```

आपको शुरू से HTTP के अन्दर जाना होगा कि कैसे यह काम करता है। Go में, हम `net/http` पैकेज का उपयोग कर सकते हैं। उस कॉम्पोनेंट का उपयोग करें जो आप server पर अपने अनुरोध को भेजना चाहे। आप URL, मेथड (GET, POST आदि) और कोई पैरामीटर्स भी भेज सकते हैं। आप अपने अनुरोध का जवाब प्राप्त करने के लिए इस रिस्पॉन्स को किसी भी वेरिएबल में स्टोर कर सकते हैं। तो यही कोई मामूली उदाहरण है, लेकिन आप इसे अपने एप्लिकेशन के अनुसार बदल सकते हैं।

## गहराई में जाएँ:
HTTP अनुरोध को भेजने का प्रयोग 1990 के दशक में शुरू हुआ था। इसे साधारण भाषा में कहा जाता है, जहाँ अमरीकी इकाई ने वेब साइट बनाने के लिए CGI का उपयोग करना चाहा। अन्य विकल्पों में POST, PUT और DELETE जैसे HTTP मेथड भी हैं। Go में, हम `http.NewRequest()` फ़ंक्शन गहराई में गए हैं जहाँ हम स्वयं अपने अनुरोध में पैरामीटर्स भेज सकते हैं। यह भी ध्यान दें की Go आपके अनुरोध को स्वचालित रूप से प्रसंस्करण करेगा।

## देखें भी:
- Go की `net/http` पैकेज डोक्युमेंटेशन: https://golang.org/pkg/net/http/
- Go की `http.Request` स्ट्रक्चर: https://golang.org/pkg/net/http/#Request
- HTTP अनुरोध का उदाहरण: https://golang.org/pkg/net/http/#example_Request--Get