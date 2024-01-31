---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:46.228894-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Text file पढ़ना, यानी की सादे टेक्स्ट की सामग्री को कोड से सीधे निकालना। Programmers इसे क्यों करते हैं? कोड में सेटिंग्स, डेटा या configurations load करने के लिए।

## How to: (कैसे करें:)

Basic टेक्स्ट फ़ाइल पढ़ने के लिए Go कोड:

```Go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```
अगर `example.txt` में लिखा है "नमस्ते Go!", तो output ऐसा होगा:

```
नमस्ते Go!
```

## Deep Dive (गहराई से जानकारी)

पहले, C या UNIX जैसी पुरानी टेक्नोलॉजीज में भी text files प्रमुख डेटा एक्सचेंज का माध्यम थीं। Go में `io` और `bufio` पैकेज फ़ाइल पढ़ने को आसान और प्रभावी बनाते हैं। `os.Open` से फ़ाइल खोली जाती है, `bufio.NewScanner` से content को पढ़ा जाता है, और `defer file.Close()` से फ़ाइल को बाद में सही तरीके से बंद किया जाता है। वैकल्पिक तरीकों में `ioutil.ReadFile` और `os.ReadFile` शामिल हैं, जो पूरी फ़ाइल को एक बार में memory में लोड करते हैं।

## See Also (देखें भी)

- Go by Example पर File Reading: https://gobyexample.com/reading-files
- Go डॉक्स में `ioutil` पैकेज: https://pkg.go.dev/io/ioutil
- Go डॉक्स में `bufio` पैकेज: https://pkg.go.dev/bufio
