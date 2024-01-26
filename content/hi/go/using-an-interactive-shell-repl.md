---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:15:27.793169-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक REPL (Read-Eval-Print Loop) आपको लाइव कोड के साथ इंटरैक्ट करने देता है; यह इनपुट को पढ़ता है, मूल्यांकन करता है, परिणाम प्रिंट करता है, और वापस लूप में जाता है। प्रोग्रामर इसका उपयोग स्निपेट्स को टेस्ट करने, डीबग करने, और रियल-टाइम में नई भाषाएँ सीखने के लिए करते हैं।

## कैसे:
Go में एक बिल्ट-इन REPL शामिल नहीं है, लेकिन आप तृतीय-पक्ष टूल्स का उपयोग कर सकते हैं। एक लोकप्रिय टूल है `gore`:

```go
// gore को इंस्टॉल करें
$ go install github.com/motemen/gore/cmd/gore@latest

// gore चलाएँ
$ gore
gore version 0.5.0  :help for help
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## गहन विवेचना
मूल रूप से Lisp के लिए विकसित किया गया, REPLs डायनामिक भाषाओं जैसे कि Python या Ruby में आम हैं। Go, स्टेटिकली टाइप्ड होने के नाते, इसे आउट-ऑफ-द-बॉक्स शामिल नहीं करता है। `gore` के विकल्पों में `go-pry` और `yaegi` शामिल हैं। ये टूल्स Go कोड का निरूपण करते हैं, जिससे आप बिना एक पूर्ण-विकसित ऐप को कम्पाइल किए तेजी से विचारों का पता लगा सकते हैं और मान्य कर सकते हैं। वे विशेष रूप से शुरुआती लोगों और शैक्षिक संदर्भों में उपयोगी होते हैं, जहाँ पर ध्यान सीखने और प्रयोग करने पर होता है।

## देखें भी
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)