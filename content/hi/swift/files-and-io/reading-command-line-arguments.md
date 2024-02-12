---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases: - /hi/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:57:11.512780-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन अर्ग्यूमेंट्स पढ़ना यानी यूजर से सीधे इनपुट लेना होता है जब प्रोग्राम चल रहा हो। प्रोग्रामर्स यह करते हैं ताकि उनके प्रोग्राम फ्लेक्सिबल और इंटरैक्टिव हों।

## How to: (कैसे करें:)
```Swift
// main.swift
// सिम्पल कोड जो कमांड लाइन अर्ग्यूमेंट्स डिस्प्ले करता है

// CommandLine.arguments में सभी अर्ग्यूमेंट्स स्टोर होते हैं
for argument in CommandLine.arguments {
    print(argument)
}

// सैंपल आउटपुट:
// प्रोग्राम ./myProgram को चलाएं जिसके साथ कुछ अर्ग्यूमेंट्स दें: 
// ./myProgram Hello World!
// आउटपुट होगा:
// ./myProgram
// Hello 
// World!
```

## Deep Dive (गहराई में जानकारी):
कमांड लाइन अर्ग्यूमेंट्स का इस्तेमाल 1960s के शुरुआती कम्प्यूटर इंटरफेस से हो रहा है। Swift में `CommandLine` की एक बिल्ट-इन क्लास है, जो इसमें सहायता करती है। इसके बदले में, आप Swift Argument Parser लाइब्रेरी का भी इस्तेमाल कर सकते हो, जिससे कोड और भी स्ट्रक्चर्ड और आसान हो जाता है। Command Line Arguments को पढ़ते समय इम्प्लीमेंटेशन डिटेल्स जैसे कि इनपुट वैलिडेशन, इंडेक्सिंग एरर्स और सिक्योरिटी कॉन्सिडरेशन्स ध्यान में रखने जरूरी होते हैं।

## See Also (सम्बंधित स्रोत):
- [Swift.org Command Line Tool Documentation](https://swift.org/package-manager/#example-usage)
- [GitHub Swift Argument Parser](https://github.com/apple/swift-argument-parser)
- [Ray Wenderlich CommandLine Tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial)
