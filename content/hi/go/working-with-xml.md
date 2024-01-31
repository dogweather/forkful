---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:32:29.599306-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ कार्य करना कोड का उपयोग करके XML दस्तावेज़ों को पार्स करना, बनाना और मैनिपुलेट करना शामिल है। प्रोग्रामर इसे डेटा इंटरचेंज, कॉन्फ़िग फाइलों और वेब सेवाओं के लिए करते हैं क्योंकि XML की पठनीयता और व्यापक समर्थन इसे संरचित डेटा के लिए एक मजबूत विकल्प बनाते हैं।

## कैसे करें:
Go में, `encoding/xml` पैकेज का उपयोग करें। आइए XML को पार्स और जेनरेट करते हैं।
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs XML तत्वों के लिए मानचित्र बनाते हैं
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Marshal struct को XML में
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML को struct में
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
नमूना आउटपुट:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## गहराई से खोज
XML 90 के दशक के अंत में आया था, बड़े पैमाने पर इलेक्ट्रॉनिक प्रकाशन के लिए डिज़ाइन किया गया था लेकिन जल्दी से वेब के लिए अपनाया गया। JSON जैसे विकल्प उठ खड़े हुए हैं, सरलता के लिए प्रचारित किए गए हैं, लेकिन XML के दस्तावेज़ सत्यापन के माध्यम से योजनाएँ और नेमस्पेस जटिल दस्तावेज़ों के लिए शक्तिशाली रहते हैं। Go में, `encoding/xml` अधिकांश कार्यों को संभालता है, लेकिन विशाल दस्तावेज़ों या स्ट्रीम प्रोसेसिंग के लिए, निम्न-स्तरीय नियंत्रण और बेहतर प्रदर्शन के लिए `xml.NewDecoder` और `xml.NewEncoder` पर विचार करें।

## भी देखें
- Go का `encoding/xml` पैकेज: https://pkg.go.dev/encoding/xml
- XML ट्यूटोरियल: https://www.w3schools.com/xml/
- Go ब्लॉग पर XML: https://blog.golang.org/xml
- JSON और XML के बीच तुलना: https://www.json.org/xml.html
