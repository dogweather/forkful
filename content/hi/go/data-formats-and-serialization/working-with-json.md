---
title:                "JSON के साथ काम करना"
aliases:
- /hi/go/working-with-json/
date:                  2024-02-03T18:13:05.741459-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Go में JSON (JavaScript Object Notation) के साथ काम करना Go डेटा संरचनाओं और JSON प्रारूप के बीच डेटा को एन्कोडिंग और डिकोडिंग करने की प्रक्रिया को शामिल करता है। यह कार्य वेब सेवाओं और APIs में सर्वव्यापी है, क्योंकि JSON एक हल्का, पाठ-आधारित, और भाषा-स्वतंत्र डेटा आदान-प्रदान प्रारूप के रूप में कार्य करता है, जो विभिन्न प्रोग्रामिंग पर्यावरणों में सरल डेटा साझाकरण को सक्षम बनाता है।

## कैसे करें:

Go में, `encoding/json` पैकेज आपको JSON संचालन का द्वार प्रदान करता है, Go डेटा संरचनाओं को JSON में बदलने (मार्शलिंग) और वापस (अनमार्शलिंग) करने की तंत्र प्रदान करते हैं। नीचे आपको शुरू करने के लिए मूल उदाहरण दिए गए हैं:

### एन्कोडिंग (मार्शलिंग)

Go संरचना को JSON में बदलने के लिए, आप `json.Marshal` का उपयोग कर सकते हैं। निम्न Go संरचना पर विचार करें:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

आउटपुट:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### डिकोडिंग (अनमार्शलिंग)

Go डेटा संरचना में JSON पार्स करने के लिए `json.Unmarshal` का उपयोग करें:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

पहले की तरह `User` संरचना दी गई, यह कोड JSON स्ट्रिंग को एक उपयोगकर्ता उदाहरण में पार्स करता है।

आउटपुट:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## गहन विवेचन

Go में `encoding/json` पैकेज JSON संचालन में शामिल जटिलता को बहुत हद तक अमूर्त करने वाला एक सीधा API प्रदान करता है। Go के विकास की शुरुआती अवस्था में परिचय दिया गया, यह पैकेज Go की सादगी और कुशलता के दर्शन को प्रतिबिंबित करता है। हालांकि, `encoding/json` द्वारा रनटाइम पर संरचनाओं का निरीक्षण और संशोधन करने के लिए रिफ्लेक्शन का उपयोग CPU-गहन परिदृश्यों में अनुकूल से कम प्रदर्शन की ओर ले जा सकता है।

`json-iterator/go` और `ffjson` जैसे विकल्प सामने आए हैं, जो स्थिर मार्शलिंग और अनमार्शलिंग कोड उत्पन्न करके तेज JSON प्रोसेसिंग प्रदान करते हैं। हालांकि, `encoding/json` सादगी, मजबूति के कारण और तथ्य यह है कि यह मानक पुस्तकालय का हिस्सा है, सबसे अधिक सामान्यतः इस्तेमाल किया जाने वाला पैकेज बना हुआ है, जो Go संस्करणों में संगतता और स्थिरता सुनिश्चित करता है।

इसके धीमे सापेक्ष प्रदर्शन के बावजूद, उपयोग में आसानी और Go की प्रकार प्रणाली के साथ एकीकरण `encoding/json` को अधिकांश अनुप्रयोगों के लिए उपयुक्त बनाता है। प्रदर्शन महत्वपूर्ण होने वाले संदर्भों में काम करने वालों के लिए, बाहरी पुस्तकालयों की खोज करना सार्थक हो सकता है, लेकिन कई के लिए, मानक पुस्तकालय गति, सादगी, और विश्वसनीयता के बीच सही संतुलन बनाता है।
