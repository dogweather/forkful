---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:53:13.135127-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डिबग आउटपुट प्रिंट करने का मतलब अपने कोड में सूचनाएँ लॉग करना है। प्रोग्रामर इसे इसलिए करते हैं ताकि समस्याओं की पहचान करने और समाधान करने में आसानी हो।

## How to: (कैसे करें:)
```Go
package main

import (
    "fmt"
    "log"
    "os"
)

func main() {
    // एक साधारण संदेश प्रिंट करना
    fmt.Println("यह साधारण प्रिंट स्टेटमेंट है")

    // लॉगर सेटअप करना
    logger := log.New(os.Stdout, "DEBUG: ", log.Ldate|log.Ltime|log.Lshortfile)

    // डिबग संदेश लॉग करना
    logger.Println("यह डिबग संदेश है")
}
```

सैंपल आउटपुट:
```
यह साधारण प्रिंट स्टेटमेंट है
DEBUG: main.go:15: यह डिबग संदेश है
```

## Deep Dive (गहराई से अध्ययन)
प्रोग्रामिंग में डिबग आउटपुट का इतिहास 1950 के दशक तक जाता है जब पहले कंप्यूटर्स में बग्स का पता लगाना एक चुनौती थी। अल्टरनेटिव्स जैसे कि IDEs के बिल्ट-इन डिबगर्स या जटिल लॉगिंग फ्रेमवर्क्स का इस्तेमाल किया जा सकता है लेकिन सिंपल स्टेटमेंट्स हमेशा त्वरित समाधान प्रदान करते हैं। Go में `fmt` पैकेज का इस्तेमाल साधारण प्रिंटिंग के लिए और `log` पैकेज ज्यादा डिटेल्ड लॉगिंग ऑप्शन्स के लिए होता है।

## See Also (और भी जानकारी)
- Go लॉगिंग पैकेज डॉक्युमेंटेशन: [log package - log - pkg.go.dev](https://pkg.go.dev/log)
- एक इफेक्टिव गो प्रोग्रामिंग गाइड: [Effective Go - golang.org](https://golang.org/doc/effective_go)
- How to write log output to a file in Go: [Logging to a file in Go - Stack Overflow](https://stackoverflow.com/questions/19965795/how-to-write-log-output-to-a-file-in-go)
