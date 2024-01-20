---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-html.md"
---

{{< edit_this_page >}}

## "क्या और क्यों?"

HTML पार्सिंग (parsing) एक प्रक्रिया है जिसमें एक HTML डॉक्यूमेंट को उसके संगठनात्मक संरचना में विभाजित किया जाता है। प्रोग्रामर्स इसे उपयोग करते हैं ताकि वे वेब पेजेस के विभिन्न हिस्सों (जैसे कि शीर्षक, पैराग्राफ, लिंक, इत्यादि) के उपयोग और संपादन को स्वचालित कर सकें।

## "कैसे:"

आप Go से HTML पार्स कर सकते हैं गो नेट / HTML पैकेज का उपयोग करके। निम्नलिखित कोड संकेत देता है कि कैसे:

```Go
इंपॉर्ट (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)
  
func main() {
    res, err := http.Get("http://www.google.com")
    if err != nil {
        fatal("%s", err)
    }
  
    doc, err := html.Parse(res.Body)
    if err != nil {
        fatal("%s", err)
    }
  
    fmt.Println(doc)
}
```

ये कोड ब्लॉक Google के होमपेज की HTML डॉक्यूमेंट को पार्स करेगा और पार्स किये गए संरचना को प्रिंट करेगा।

## "गहरी डाइव"

HTML पार्सिंग का इस्तेमाल वेब डेवलपमेंट की शुरुआत से ही किया जा रहा है। पार्सिंग से वेब पेजेस के डाटा को एक्स्ट्रैक्ट करने या विश्लेषित करने के लिए दानवादी प्रक्रियाएं और फ़ंक्शन्स बना सकते हैं।

Go के अलावा, भाषाएं जैसे कि Python और JavaScript भी HTML पार्सिंग समर्थन करती हैं, जिनमें पार्सर्स जैसे BeautifulSoup और Cheerio शामिल हैं। 

Go का `net/html` पैकेज, जो Golang का कोर लाइब्रेरी का हिस्सा है, उसके DOM-like पार्सिंग समर्थन के साथ-साथ HTML5 स्पेसिफिकेशन में उल्लिखित विभिन्न 'नोड' का समर्थन करता है।

## "और भी देखें:"

1. Go द्वारा HTML पार्सिंग: [Link](https://golang.org/pkg/net/html/)
2. Python BeautifulSoup HTML पार्सिंग: [Link](https://beautiful-soup-4.readthedocs.io/)
3. JavaScript Cheerio HTML पार्सिंग: [Link](https://cheeriojs.github.io/cheerio/)