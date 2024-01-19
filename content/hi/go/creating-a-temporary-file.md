---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अस्थायी फाइल बनाना एक क्रिया होती है जिससे आपके पास कुछ समय के लिए एक फाइल की उपलब्धता होती है, जिसको आप बाद में मिटा सकते हैं। प्रोग्रामर इसे डाटा क्षेत्रीयता और डिबगिंग के लिए करते हैं।

## कैसे करें:

आप निम्नलिखित कोड उदाहरण के माध्यम से गो में अस्थायी फ़ाइल बना सकते हैं:

```Go
package main

import (
   "io/ioutil"
   "log"
)

func main() {
  tempFile, err := ioutil.TempFile("temp", "test")
  if err != nil {
    log.Fatal(err)
  }

  log.Printf("A temporary file has been created: %s", tempFile.Name())

  defer func() {
    tempFile.Close()
    os.Remove(tempFile.Name())
  }()
}
```

जब यह कोड चलता है, तो आपको अपने लॉग में अस्थायी फ़ाइल नाम मिलता है।

## गहराई डाइव

गो में अस्थायी फ़ाइलें अवधारणा UNIX सिस्टम से ली गई हैं। ये इससे अलग हैं क्योंकि वे अत्यधिक क्षेत्रीय हैं और सिस्टम रिसोर्स का अच्छा उपयोग करते हैं।

वैकल्पिक तरीके `os.CreateTemp()` जैसी कार्यवाही कर सकते हैं, लेकिन `ioutil.TempFile()` नाम और पथ निर्माण के साथ-साथ फ़ाइल को खोलने का काम भी करती है।

## यह भी देखें:

1. [ioutil गो डॉक्यूमेंटेशन](https://golang.org/pkg/io/ioutil/#TempFile)
2. [os पैकेज गो डॉक्यूमेंटेशन](https://golang.org/pkg/os/#CreateTemp)