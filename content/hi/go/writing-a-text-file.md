---
title:                "Go: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें सिस्टम में डेटा को स्टोर करने की आवश्यकता होती है और उसे अगले उपयोग के लिए सुरक्षित रूप से रखना होता है। जैसे कि एक लॉग फ़ाइल या सेटिंग्स फ़ाइल। यहां हम जानेंगे कि गो प्रोग्रामिंग में कैसे टेक्स्ट फ़ाइल लिखा जाता है।

## कैसे करें

```
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    data := []byte("यह टेक्स्ट फ़ाइल में लिखा गया है।")

    err := ioutil.WriteFile("example.txt", data, 0644)
    if err != nil {
        fmt.Println(err)
    }
}
```

इस उदाहरण में, हमने एक `[]byte` और `ioutil.WriteFile()` के साथ पाठ को लिखने के लिए फ़ाइल का नाम और अनुमति को दिया है। कोड को रन किया जाता है तो `example.txt` नाम की नई फ़ाइल बनाई जाती है और पाठ उसमे लिख दिया जाता है।

## डीप डाइव

अधिक विस्तृत जानकारी के लिए, गो प्रोग्रामिंग भाषा में `fmt`, `os` और `io/ioutil` पैकेज से संबंधित फ़ंक्शन और मेथड के बारे में जान सकते हैं। आप इन पैकेजों में से समानिता हैं।

## देखें भी

- [गो प्रोग्रामिंग भाषा](https://en.wikipedia.org/wiki/Go_(programming_language))
- [जावा में फाइल लिखाना](https://www.geeksforgeeks.org/different-ways-for-writing-a-file-in-java/)