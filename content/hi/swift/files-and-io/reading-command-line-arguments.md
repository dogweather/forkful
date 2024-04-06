---
date: 2024-01-20 17:57:11.512780-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:54.895479-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

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
