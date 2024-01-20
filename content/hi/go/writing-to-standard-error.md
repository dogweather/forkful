---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Standard error (stderr) एक output stream है जिसके जरिए हम गलतियों और logs को print करते हैं. इसका इस्तेमाल आमतौर पर program की गलतियां दिखाने के लिए किया जाता है, ताकि वो standard output (stdout) से अलग रहें।

## How to: (कैसे करें:)

Go में `os` पैकेज का इस्तेमाल करके हम stderr में लिख सकते हैं:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    if _, err := fmt.Fprintf(os.Stderr, "यह एक त्रुटि संदेश है\n"); err != nil {
        panic(err)
    }
}
```

अगर आप इसे रन करेंगे तो आपको terminal में नीचे दिखाया गया output मिलेगा:

```
यह एक त्रुटि संदेश है
```

## Deep Dive (गहराई में जानकारी)

Standard error और standard output दोनों ही Unix की विरासत से आते हैं, जिसमें अलग-अलग streams के जरिए data को handle करने की व्यवस्था होती है। Logs और errors को stderr में लिखने का मतलब है कि ये सामान्य output से अलग जा सकते हैं, जिससे उन्हें आसानी से पहचाना और मैनेज किया जा सकता है। Go में `log` पैकेज भी है जो डिफ़ॉल्ट रूप में stderr में लिखता है और अधिक जटिल logging capabilities प्रदान करता है।

## See Also (और भी देखें)

- Go दस्तावेज़ीकरण `fmt` पैकेज: https://pkg.go.dev/fmt
- Go दस्तावेज़ीकरण `log` पैकेज: https://pkg.go.dev/log
- Unix philosophy के बारे में जानकारी: https://en.wikipedia.org/wiki/Unix_philosophy