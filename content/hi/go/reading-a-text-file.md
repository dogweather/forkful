---
title:                "टेक्स्ट फाइल को पढ़ना"
html_title:           "Go: टेक्स्ट फाइल को पढ़ना"
simple_title:         "टेक्स्ट फाइल को पढ़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun
Is article ka maksad aapko sikhana hai ki kaise aap Go programming language mein ek text file ko read kar sakte hain. Agar aap code karne mein interested hain ya fir aapko keval curiosity hai to bhi yeh article aapke liye helpful hoga.

## Kaise
```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Text file ka path define karna
    path := "example.txt"

    // File ko read karne ke liye ioutil ka use karna
    content, err := ioutil.ReadFile(path)
    if err != nil {
        fmt.Println("Error while reading file:", err)
    }

    fmt.Println(string(content)) // File ke content ko print karna
}
```

Output:
```
Hello, world!
This is an example text file.
```

## Deep Dive
Text files ko read karna Go programming language mein kaafi easy aur efficient hai. Humne ioutil package ka use kiya hai kyunki yeh file ki content ko string format mein return karta hai. Lekin agar aapke paas file ka koi aur format hai, jaise CSV ya JSON, toh aapko corresponding package ka use karna hoga.

## Dekhiye Bhi
- [Official Go Documentation on File Input/Output](https://golang.org/pkg/io/ioutil/)
- [Tutorial on Reading and Writing Files in Go](https://tutorialedge.net/golang/reading-and-writing-files-in-go/)