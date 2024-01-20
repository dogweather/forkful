---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

Command line arguments क्या होते हैं? ये कुछ परिवर्तनशील मान होते हैं जिन्हे हम एक script को run करते समय पास कर सकते हैं। प्रोग्रामर्स इसे क्यों करते हैं? प्राथमिकता का सम्पादन, प्रवेशि डेटा को customize करने के लिए और अन्य टॉस्क्स को अनुकूलित करने के लिए। 

## कैसे करें:

```TypeScript
// index.ts
let myArgs: string[] = process.argv.slice(2);
console.log('myArgs: ', myArgs);
```
उपरोक्त कोड को run करने पर, यदि आपके उस कोड के command line पर `node index.js arg1 arg2 arg3` इत्यादि input देते हैं, तो आपको निम्नलिखित output मिलेगा:

```Shell
myArgs:  [ 'arg1', 'arg2', 'arg3' ]
```

## Deep Dive:

हमेशा से ही प्रोग्रामर्स ने shell script और अन्य text-based programs में command line arguments का उपयोग किया है। यह ट्रेडिशनल तरीका है किसी script को बाहरी inputs प्रदान करने का। 

कुछ स्थानों पर आप optargs नामक library का उपयोग करके जटिल command line arguments के साथ काम कर सकते हैं। 

TypeScript में, `process.argv` array का उपयोग करके command line arguments पढ़ें। "process" एक Global object है जो Node.js environment के बारे में जानकारी प्रदान करता है, और `argv` एक array है जो command line आर्ग्युमेंट्स को माल्टी प्लैटफार्म तरीके से सहेजता है। 

## अधिक देखें:

1. [Node.js documentation for Command Line Options](https://nodejs.org/api/cli.html) 
2. [Stack Overflow on Parsing Command Line Arguments](https://stackoverflow.com/questions/4351521/how-do-i-pass-command-line-arguments) 
3. [Understanding Process in Node.js](https://nodejs.org/api/process.html)

ध्यान दें अगर आपको और विस्तृत साहित्य की आवश्यकता है, तो आप अधिक स्रोतों का अन्वेषण कर सकते हैं।