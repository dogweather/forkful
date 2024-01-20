---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, वेब पेज को ऐसे स्ट्रक्चर में बदलने का एक तरीका है, ताकि कंप्यूटर इसे प्रोसेस कर सके। यह काम ताकि प्रोग्रामर्स जान सकें कि HTML डॉक्यूमेंट में क्या है और वो इसे अपने कोड में इस्तेमाल कर सकें।

## कैसे करें:

हास्केल का `Text.HTML.DOM` पैकेज HTML पार्सिंग के लिए बहुत उपयोगी है। इसका प्रयोग हम इस प्रकार से कर सकते हैं:

```Haskell
import Text.HTML.DOM (parseLBS)
import qualified Data.Text.Lazy.IO as L

main = do
    htmlData <- L.readFile "example.html"
    let dom = parseLBS htmlData
    print dom
```
इस कोड को रन करने पर आपको HTML डॉक्यूमेंट का पार्स्ड संस्करण मिलेगा।

## गहरी चर्चा:

HTML पार्सिंग का काम 1990 के दशक से ही किया जा रहा है, जब से ही वेब ब्राउज़ेर बनाई गई है। अन्य विकल्पों में BeautifulSoup (Python) और Nokogiri (Ruby) शामिल हैं। हास्केल के `Text.HTML.DOM` पैकेज में, बाइट-स्तरीस (ByteString) का इस्तेमाल करके वेब पेज को पार्स किया जाता है।

## जाने के लिए:

यदि आप और अधिक जानना चाहते हैं, तो निम्नलिखित लिंक्स मददगार हो सकते हैं:

- [Haskell पर विस्तृत HTML पार्सिंग गाइड](http://chimera.labs.oreilly.com/books/1230000000929/ch05.html)