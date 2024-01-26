---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON का काम डाटा को संरचित करना है जिससे मशीनें और इंसान दोनों ही आसानी से समझ सकें। प्रोग्रामर्स इसे वेब APIs के डाटा को पढ़ने या भेजने के लिए इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"encoding/json"
	"fmt"
)

type User struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	// JSON Encoding (जेसन एनकोडिंग)
	user := User{Name: "अंकित", Age: 25}
	jsonData, err := json.Marshal(user)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))

	// JSON Decoding (जेसन डिकोडिंग)
	var decodedUser User
	err = json.Unmarshal(jsonData, &decodedUser)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", decodedUser)
}
```
आउटपुट:
```
{"name":"अंकित","age":25}
{Name:अंकित Age:25}
```

## Deep Dive (गहराई से जानकारी)
JSON, जो JavaScript Object Notation के लिए खड़ा है, डेटा इंटरचेंज का एक मानक फॉर्मेट है। पहली बार यह 2001 में देखने को मिला था। XML और YAML जैसे विकल्प भी हैं लेकिन JSON अपनी सादगी और पढ़ने में आसानी के कारण ज्यादा लोकप्रिय है। Go में `encoding/json` पैकेज JSON को एनकोड और डिकोड करने के लिए बहुत सहायक है और reflection का इस्तेमाल कर किसी भी Go टाइप में JSON डाटा को बदल देता है।

## See Also (और भी जानकारी)
- Go by Example: JSON: https://gobyexample.com/json
- Go `encoding/json` package documentation: https://pkg.go.dev/encoding/json
- JSON.org, JSON की विस्तृत जानकारी: https://www.json.org/json-en.html
