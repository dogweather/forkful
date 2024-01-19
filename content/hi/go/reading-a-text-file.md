---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

मूल रूप से टेक्स्ट फ़ाइल पढ़ना मतलब किसी फ़ाइल की सामग्री को ताजा करना। प्रोग्रामर्स इसे डेटा संचालन, त्रुटि रिपोर्टिंग, और लॉग रिपोर्टिंग के संदर्भ में उपयोग करते हैं।

## कैसे करें:

Go के आधिकारिक लाइब्ररी `os` और `bufio` का उपयोग करके चलने का उदाहरण देखें।

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, _ := os.Open("test.txt")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	file.Close()
}
```

इसे चलाने पर, आपको `test.txt` फाइल की सारी लाइनें मिलेंगी।

## गहरा डाइव:

1. हिस्टोरिकल कांटेक्स्ट: Go भाषा की कड़ी सुरक्षा और सरलता के कारण यह फ़ाइल पढ़ने का एक सुविधाजनक तरीका बन गया है।
2. विकल्प: `ioutil.ReadFile` का उपयोग भी कर सकते हैं, लेकिन इसे लागती हुई मेमोरी को ध्यान में रखा जाए, क्योंकि यह पूरी फ़ाइल को एक बार में लोड करता है।
3. आधारभूत विवरण: `os.Open` फ़ाइल को खोलता है, `bufio.NewScanner` एक नया स्कैनर बनाता है, `scanner.Scan` एक नई लाइन पर जाता है, और `scanner.Text` उस लाइन को प्रिंट करता है।

## देखने के लिए भी:

1. Go डॉक्यूमेंटेशन: https://golang.org/pkg/bufio/
2. 파일 읽기에 대한 블로그 포스트: https://go.dev/play/p/3W69TH4nON
3. Go प्रोग्रामिंग: https://tour.golang.org/welcome/1