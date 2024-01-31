---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:48.099644-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्युमेंट्स पाठना मतलब है यूजर जो इनपुट टर्मिनल में दे, उसे हमारा प्रोग्राम पढ़े। इसे हम इसलिए करते हैं ताकि यूजर डायनेमिकली इनपुट दे सके, और प्रोग्राम उसे कॉन्फ़िगर या कस्टमाइज कर सके।

## How to (कैसे करें):
Go में कमांड लाइन आर्ग्युमेंट्स पढ़ने के लिए `os.Args` वेरिएबल का इस्तेमाल होता है:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:] // पहला आर्ग्युमेंट प्रोग्राम का नाम होता है, उसे छोड़ दें
    fmt.Println("Command Line Arguments:", args)
}

// इसे रन करने के लिए टर्मिनल में लिखें:
// go run main.go arg1 arg2
// सैंपल आउटपुट:
// Command Line Arguments: [arg1 arg2]
```

## Deep Dive (गहराई में):
कमांड लाइन आर्ग्युमेंट्स का इतिहास UNIX सिस्टम्स से शुरू होता है। Go में `os` पैकेज के आर्ग्युमेंट्स हैंडल करने की क्षमता फ्लेक्सिबल और सिंपल है। अल्टरनेटिव्स में `flag` पैकेज आता है जो कॉम्प्लेक्स कमांड लाइन पार्सिंग सपोर्ट करता है। `os.Args` एक स्लाइस होती है जिसका फर्स्ट एलिमेंट प्रोग्राम का पाथ होता है, इसीलिए आमतौर पर इंडेक्स 1 से पाठना शुरू करते हैं।

## See Also (और भी देखें):
- Go डॉक्युमेंटेशन `os` पैकेज: https://pkg.go.dev/os
- Go by Example में `flag` पैकेज उपयोग: https://gobyexample.com/command-line-flags
- कमांड लाइन आर्ग्युमेंट्स पाठने की बेस्ट प्रैक्टिस के लिए आर्टिकल: https://dave.cheney.net/2014/09/14/go-programming-cookbook-command-line-arguments
