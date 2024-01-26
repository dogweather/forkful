---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:23.958791-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Command line arguments वो चीजें होती हैं जो हम अपने प्रोग्राम को रन करते समय console में दे सकते हैं। Programmers इन्हें इसलिए पढ़ते हैं ताकि उपयोगकर्ता ऐप को कस्टमाइज करने के लिए inputs वक्त पर दे सकें।

## How to: (कैसे करें:)
Node.js का उपयोग करके command line arguments पढ़ना बड़ा आसान है। नीचे दिए गए कोड का प्रयोग करें:

```Javascript
// index.js
// process.argv एरे के साथ काम करना
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// रन करने के लिए कमांड
// node index.js hello world

// आउटपुट:
// 0: /path/to/your/node/executable
// 1: /path/to/your/index.js
// 2: hello
// 3: world
```
आप इसे `slice()` फंक्शन की मदद से आसानी से इस्तेमाल कर सकते हैं:

```Javascript
// values से पहले के unnecessary elements को हटाना
const args = process.argv.slice(2);
console.log(args); // ['hello', 'world']
```

## Deep Dive (गहराई से जानकारी):
Node.js में command line arguments का पढ़ना `process.argv` के द्वारा शुरू हुआ था। यह एक array है जिसमें रनटाइम environment, फाइल पाथ और यूजर से दिए गए inputs शामिल होते हैं।

Alternatives के रूप में, कई third-party libraries जैसे कि `yargs` और `commander` भी मौजूद हैं, जो कि इस काम को और भी सरल बना देते हैं, खासकर जब complex argument parsing की बात आती है।

जब आप `process.argv` का उपयोग करते हैं, तो आपको arguments के सही format को हैंडल करने की जरूरत होती है, जैसे कि flags या key-value pairs को पहचानना। Libraries इसे आसान बनाती हैं और ज्यादातर boilerplate code को बचाती हैं।

## See Also (और जानकारी के लिए):
- Node.js documentation on process.argv: [Node.js process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- Yargs Library: [Yargs GitHub](https://github.com/yargs/yargs)
- Commander.js: [Commander GitHub](https://github.com/tj/commander.js)
