---
title:                "Go: html को पार्स करना"
simple_title:         "html को पार्स करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों
पाठकों को समझना चाहिए कि HTML पार्सिंग क्यों महत्वपूर्ण है और उन्हें इसे सीखने का फायदा क्या हो सकता है। यह उनको आपकी पोस्ट को पढ़ने के लिए प्रेरित करना चाहिए।

## कैसे करें
"```Go
func main() {
    // HTML पार्सिंग को दर्शाता है
    var html = `
    <html>
        <head>
            <title>मेरा ब्लॉग</title>
        </head>
        <body>
            <h1>यह एक उदाहरण है</h1>
        </body>
    </html>
    `
    // वेब पेज को पार्स करने के लिए एक ऑब्जेक्ट बनाएं
    dom, err := htmlquery.Parse(strings.NewReader(html))
    if err != nil {
        log.Fatal(err)
    }
    // शीर्षक को खोजें
    title := htmlquery.FindOne(dom, "//title/text()").Data
    fmt.Println(title) // "मेरा ब्लॉग"
}
```

## गहराई में जाइए
HTML पार्सिंग के बारे में और गहराई की जानकारी प्राप्त करने के लिए, आप Go की आधिकारिक डॉक्यूमेंटेशन के [पार्सर पैकेज](https://godoc.org/golang.org/x/net/html) पर जा सकते हैं। वहाँ आपको HTML पार्स करने के विभिन्न तरीके और उनका उपयोग सीखने को मिलेंगे।

## देखें भी
यदि आप Go में वेब स्क्रैपिंग करने के लिए और अधिक उदाहरण और ट्यूटोरियल खोजना चाहते हो, तो आप [GitHub पर goquery प्रोजेक्ट](https://github.com/PuerkitoBio/goquery) का उपयोग कर सकते हैं। यह आपको HTML पार्सिंग के साथ कई अन्य फीचर्स भी प्रदान करता है।