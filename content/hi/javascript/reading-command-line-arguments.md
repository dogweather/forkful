---
title:                "Javascript: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि जैसे कि हम वेबसाइटों से डेटा प्राप्त करते हैं, क्या आप भी अपने Javascript कोड में उपयोगकर्ताओं से जानकारी प्राप्त कर सकते हैं? हां, यह संभव है और इसका नाम है "Command Line Arguments". आपको जानना होगा कि यह क्या है और क्यों इसका उपयोग जानकारियों को प्राप्त करने के लिए किया जाता है.

## कैसे करें

यदि आप अपने Javascript कोड में command line arguments का उपयोग करना चाहते हैं, तो आपको सबसे पहले प्रोसेस ऑब्जेक्ट को इंस्टेंसिएट करना होगा। इसके बाद, `process.argv` में आपको सभी command line arguments मिल जाएंगे। आइए इसे एक उदाहरण के साथ समझते हैं:

```Javascript
// यहां आपको यह एक सादा सा उदाहरण दिखाया गया है जिसमें सिर्फ दो command line arguments पास किए गए हैं।

// कमांड लाइन पर यह दोनों आउटपुट प्रिंट होंगे:
// node index.js hello world

console.log(process.argv[0]); // प्रोग्राम का नाम - node
console.log(process.argv[1]); // फ़ाइल का नाम - index.js
console.log(process.argv[2]); // पहला argument - hello
console.log(process.argv[3]); // दूसरा argument - world
```

मिलेंगे, होने ही है command line arguments का दूसरा उपयोग है प्रोग्राम के चालक में मौजूद विभिन्न चयन या सेटिंग्स को प्रभावित करना। यह अधिक उदाहरण देखते हैं:

```Javascript
// यहां हम दो command line arguments ले रहे हैं - पहला एक सादा दिशा निर्देश (direction) है, जो या तो right या left हो सकता है। दूसरा उपयोगर्ता द्वारा निर्दिष्ट प्रवेशक (delimiter) है, जो कोड में उपयोग किए जाने वाले विश