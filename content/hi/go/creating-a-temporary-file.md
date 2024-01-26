---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:40:43.730002-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
टेम्पररी फाइल बनाना अस्थायी डेटा स्टोरेज है। प्रोग्रामर्स टेस्टिंग, डेटा प्रोसेसिंग, या अस्थायी बैकअप के लिए ऐसा करते हैं।

## कैसे करें? (How to:)
```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // टेम्पररी फाइल बनाएं
    tmpfile, err := ioutil.TempFile("", "example")
    if err != nil {
        panic(err)
    }

    // जरूरी होने पर डेटा लिखें
    _, err = tmpfile.Write([]byte("यह टेस्ट कंटेंट है"))
    if err != nil {
        panic(err)
    }

    // फाइल नाम दिखाएं
    fmt.Println("टेम्पररी फाइल नाम:", tmpfile.Name())

    // साफ़ करने से पहले फाइल बंद करें
    defer os.Remove(tmpfile.Name()) // फाइल डिलीट
    if err := tmpfile.Close(); err != nil {
        panic(err)
    }
}
```

सैंपल आउटपुट:
```
टेम्पररी फाइल नाम: /tmp/example123456
```

## गहराई से जानकारी (Deep Dive)
इतिहास में, टेम्पररी फाइलें डिस्क स्पेस बचाने के लिए इस्तेमाल होती थीं। गो (Go) में `ioutil.TempFile` फंक्शन सुरक्षित, अनूठी टेम्पररी फाइल बनाता है। इसे OS-specific temporary directory में स्टोर करता है। विकल्पों में आप `os` पैकेज का `os.CreateTemp` भी उपयोग कर सकते हैं, या अपनी डायरेक्टरी मैनेज कर सकते हैं।

## सम्बंधित सोर्सेज (See Also)
- Go by Example: Temporary Files and Directories: https://gobyexample.com/temporary-files-and-directories
- गो डॉक्यूमेंटेशन (Go Documentation) `ioutil` पैकेज: https://pkg.go.dev/io/ioutil
- गो डॉक्यूमेंटेशन (Go Documentation) `os` पैकेज: https://pkg.go.dev/os

ये स्रोत आपको ज्यादा उदहारण और टेम्पररी फाइल हैंडलिंग के बारे में विस्तृत जानकारी देंगे।
