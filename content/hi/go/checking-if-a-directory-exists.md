---
title:                "डायरेक्टरी मौजूद है या नहीं यह जांचना"
html_title:           "Go: डायरेक्टरी मौजूद है या नहीं यह जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं यह जांचना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?

डायरेक्टरी मौजूद है या नहीं जांचने कैसे समिधान किया जाता है, यह एक साधारण पुरीक्रिया है जिसका उपयोग कार्यक्रमकर्ता करते हैं। किसी भी त्रुटि को बचने के लिए या डायरेक्टरी में फ़ाइलें पढ़ने या लिखने से पहले, यह आवश्यक है कि हम यह सुनिश्चित करें कि डायरेक्टरी वास्तव में मौजूद है।

## कैसे करें:

नहीं जांचने के लिए आप `os` पैकेज का उपयोग कर सकते हैं। 

```Go
package main

import (
	"fmt"
	"os"
)

func dirExists(d string) bool {
	_, err := os.Stat(d)
	if os.IsNotExist(err) {
		return false
	}
	return err == nil
}

func main() {
	fmt.Println(dirExists("/home"))
}
```

यह कार्यक्रम `/home` नामक डायरेक्टरी मौजूद है या नहीं, यह चेक करने के लिए `dirExists` फ़ंक्शन का उपयोग करेगा। 

## गहराई में:

डायरेक्टरी मौजूद है या नहीं जांचने के लिए, `os.Stat` और `os.IsNotExist` फ़ंक्शन्स का उपयोग किया जाता है। `os.Stat` फ़ंक्शन फ़ाइल या डायरेक्टरी की जानकारी प्राप्त करता है, और यदि ऐसा कुछ नहीं मिलता है, तो एरर देता है। यह एरर `os.IsNotExist` के द्वारा जांचा जाता है।

वैकल्पिक रूप से, आप `ioutil.ReadDir` या `filepath.Walk` का भी उपयोग कर सकते हैं, लेकिन इन्हें ज्यादातर उच्च स्तर की जांच के लिए इस्तेमाल किया जाता है।

## भी देखें:

- [Go by Example: Directories](https://gobyexample.com/directories)
- [os package - The Go Programming Language](https://golang.org/pkg/os/)
- [ioutil package - The Go Programming Language](https://golang.org/pkg/io/ioutil/)
- [filepath package  - The Go Programming Language](https://golang.org/pkg/path/filepath/)