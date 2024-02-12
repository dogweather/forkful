---
title:                "XML के साथ काम करना"
aliases: - /hi/go/working-with-xml.md
date:                  2024-02-03T18:14:11.352086-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Go में XML के साथ काम करना XML दस्तावेज़ों को पार्स करने (पढ़ने) और जेनरेट करने (लिखने) की प्रक्रिया में शामिल होता है—एक मानक प्रारूप जो संरचित डेटा आदान-प्रदान के लिए होता है। प्रोग्रामर इसे डेटा स्टोरेज, कॉन्फ़िगरेशन सेटिंग्स, या सिस्टम्स के बीच डेटा एक्सचेंज के लिए करते हैं, विशेषकर उन पर्यावरणों में जहाँ XML पसंदीदा या पुरातन डेटा प्रारूप होता है।

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
