---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:11.352086-07:00
description: "Go \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E XML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\
  \u093C\u094B\u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\
  \u0947 (\u092A\u0922\u093C\u0928\u0947) \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\
  \u091F \u0915\u0930\u0928\u0947 (\u0932\u093F\u0916\u0928\u0947) \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u092E\u0947\u0902 \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u2014\u090F\u0915 \u092E\
  \u093E\u0928\u0915 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u091C\u094B \u0938\
  \u0902\u0930\u091A\u093F\u0924 \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:51.478790-06:00'
model: gpt-4-0125-preview
summary: "Go \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0930\u0928\u093E XML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u093C\
  \u094B\u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947\
  \ (\u092A\u0922\u093C\u0928\u0947) \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\u091F\
  \ \u0915\u0930\u0928\u0947 (\u0932\u093F\u0916\u0928\u0947) \u0915\u0940 \u092A\u094D\
  \u0930\u0915\u094D\u0930\u093F\u092F\u093E \u092E\u0947\u0902 \u0936\u093E\u092E\
  \u093F\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u2014\u090F\u0915 \u092E\u093E\
  \u0928\u0915 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u091C\u094B \u0938\u0902\
  \u0930\u091A\u093F\u0924 \u0921\u0947\u091F\u093E \u0906\u0926\u093E\u0928-\u092A\
  \u094D\u0930\u0926\u093E\u0928 \u0915\u0947 \u0932\u093F\u090F \u0939\u094B\u0924\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930 \u0907\u0938\u0947 \u0921\u0947\u091F\u093E \u0938\u094D\u091F\u094B\u0930\
  \u0947\u091C, \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\
  \u0928 \u0938\u0947\u091F\u093F\u0902\u0917\u094D\u0938, \u092F\u093E \u0938\u093F\
  \u0938\u094D\u091F\u092E\u094D\u0938 \u0915\u0947 \u092C\u0940\u091A \u0921\u0947\
  \u091F\u093E \u090F\u0915\u094D\u0938\u091A\u0947\u0902\u091C \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u0935\u093F\u0936\u0947\
  \u0937\u0915\u0930 \u0909\u0928 \u092A\u0930\u094D\u092F\u093E\u0935\u0930\u0923\
  \u094B\u0902 \u092E\u0947\u0902 \u091C\u0939\u093E\u0901 XML \u092A\u0938\u0902\u0926\
  \u0940\u0926\u093E \u092F\u093E \u092A\u0941\u0930\u093E\u0924\u0928 \u0921\u0947\
  \u091F\u093E \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u0939\u094B\u0924\u093E\
  \ \u0939\u0948\u0964."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे:


### Go में XML पार्सिंग
Go में XML पार्स करने के लिए, आप `encoding/xml` पैकेज का उपयोग करते हैं। यह पैकेज Go structs में XML को अनमार्शल (पार्स) करने के लिए आवश्यक उपकरण प्रदान करता है। उदाहरण के लिए, नीचे दिए गए XML डेटा को देखें, जो एक पुस्तक का प्रतिनिधित्व करता है:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

इसे पार्स करने के लिए, एक स्ट्रक्चर को परिभाषित करें जो XML संरचना का दर्पण हो:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

आउटपुट:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Go में XML जेनरेट करना
Go डेटा संरचनाओं से XML दस्तावेज़ जेनरेट करने के लिए, आप फिर से `encoding/xml` पैकेज का उपयोग करते हैं। इस बार आप Go structs को XML में मार्शल करते हैं। पिछले `Book` स्ट्रक्चर को दिया गया:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

आउटपुट:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## गहराई से विचार
XML की वाचालता और जटिलता ने कई एप्लिकेशनों के लिए JSON और अन्य प्रारूपों को अधिक लोकप्रिय बना दिया है। हालाँकि, जटिल संरचनात्मक डेटा को प्रस्तुत करने की XML की क्षमता और इसका पुरानी प्रणालियों और विशिष्ट डोमेन्स (जैसे, SOAP सेवाएँ) में व्यापक उपयोग इसकी प्रासंगिकता को सुनिश्चित करता है।

Go में `encoding/xml` पैकेज XML के साथ काम करने के लिए शक्तिशाली तंत्र प्रदान करता है, लेकिन इसकी सीमाओं का उल्लेख करना महत्वपूर्ण है। उदाहरण के लिए, XML नेमस्पेसों को हैंडल करना झंझटी हो सकता है और आसान उपयोग के मामलों की तुलना में XML स्पेसिफिकेशन की अधिक विस्तृत समझ की मांग कर सकता है। इसके अतिरिक्त, जबकि Go की स्टैटिक टाइपिंग और `encoding/xml` पैकेज की मार्शलिंग और अनमार्शलिंग क्षमताएँ आम तौर पर कुशल होती हैं, डेवलपर्स गहराई से नेस्टेड संरचनाओं के साथ या Go के टाइप सिस्टम पर सटीक रूप से मैप न करने वाले XML दस्तावेज़ों के साथ चुनौतियों का सामना कर सकते हैं।

अधिकतर आधुनिक एप्लिकेशनो�
