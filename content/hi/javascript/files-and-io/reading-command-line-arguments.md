---
date: 2024-01-20 17:56:23.958791-07:00
description: "Command line arguments \u0935\u094B \u091A\u0940\u091C\u0947\u0902 \u0939\
  \u094B\u0924\u0940 \u0939\u0948\u0902 \u091C\u094B \u0939\u092E \u0905\u092A\u0928\
  \u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\
  \u0928 \u0915\u0930\u0924\u0947 \u0938\u092E\u092F console \u092E\u0947\u0902 \u0926\
  \u0947 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 Programmers \u0907\u0928\
  \u094D\u0939\u0947\u0902 \u0907\u0938\u0932\u093F\u090F \u092A\u0922\u093C\u0924\
  \u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0909\u092A\u092F\u094B\u0917\
  \u0915\u0930\u094D\u0924\u093E\u2026"
lastmod: '2024-03-13T22:44:53.016208-06:00'
model: gpt-4-1106-preview
summary: "Command line arguments \u0935\u094B \u091A\u0940\u091C\u0947\u0902 \u0939\
  \u094B\u0924\u0940 \u0939\u0948\u0902 \u091C\u094B \u0939\u092E \u0905\u092A\u0928\
  \u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\
  \u0928 \u0915\u0930\u0924\u0947 \u0938\u092E\u092F console \u092E\u0947\u0902 \u0926\
  \u0947 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 Programmers \u0907\u0928\
  \u094D\u0939\u0947\u0902 \u0907\u0938\u0932\u093F\u090F \u092A\u0922\u093C\u0924\
  \u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0909\u092A\u092F\u094B\u0917\
  \u0915\u0930\u094D\u0924\u093E\u2026"
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
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
