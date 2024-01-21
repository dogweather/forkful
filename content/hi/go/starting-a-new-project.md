---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:04:09.433869-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना मतलब बिल्कुल नए सोफ़्टवेयर का निर्माण करना होता है। प्रोग्रामर इसे नई समस्याओं के समाधान, नए आइडिया बनाने, या सीखने के लिए करते हैं।

## How to: (कैसे करें:)
Go में नया प्रोजेक्ट शुरू करने के लिए, आपको आधारभूत सेटअप करना होता है।

```Go
package main

import "fmt"

func main() {
    fmt.Println("नमस्ते, नया प्रोजेक्ट!")
}
```

जब आप इस प्रोग्राम को रन करेंगे:

```Go
$ go run main.go
```

तब आउटपुट होगा:

```
नमस्ते, नया प्रोजेक्ट!
```

## Deep Dive (गहराई में जानकारी):
Go (या Golang) की शुरुआत Google में 2009 में हुई थी। इसे सिस्टम प्रोग्रामिंग के लिए सरल और तेज़ बनाया गया था। Go मॉड्यूल आपको परियोजनाओं का प्रबंधन करने देते हैं, यह 2018 में Go 1.11 के साथ शुरू हुआ था। पुराने तरीकों में GOPATH का उपयोग होता था, लेकिन अब मॉड्यूल से काम आसान हो गया है। एक नए प्रोजेक्ट की शुरुआत के लिए `go mod init myproject` कमांड से परियोजना का नाम देकर शुरू करें। संगठन और पैकेज प्रबंधन महत्वपूर्ण हैं और Go की अपनी वर्कस्पेस व्यवस्था 'Modules' से यह आसान हो जाता है।

## See Also (और भी जानें):
- Go की औपचारिक डॉक्यूमेंटेशन: [https://golang.org/doc/](https://golang.org/doc/)
- Go by Example के माध्यम से बेहतर समझ: [https://gobyexample.com/](https://gobyexample.com/)
- Go Modules के बारे में अधिक सीखें: [https://blog.golang.org/using-go-modules](https://blog.golang.org/using-go-modules)