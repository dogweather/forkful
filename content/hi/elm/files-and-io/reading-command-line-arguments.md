---
date: 2024-01-20 17:56:07.654721-07:00
description: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\
  \u094D\u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u095D\u0928\u093E\
  \ \u092F\u0939 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u0915\u093F \u0906\u092A\
  \ \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \ \u0915\u094B \u0936\u0941\u0930\u0942 \u0915\u0930\u0924\u0947 \u0935\u0915\u094D\
  \u0924 \u0909\u0938\u0947 \u0905\u0924\u093F\u0930\u093F\u0915\u094D\u0924 \u091C\
  \u093E\u0928\u0915\u093E\u0930\u0940 \u0926\u0947 \u0938\u0915\u0947\u0902\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u094B\u2026"
lastmod: '2024-03-13T22:44:52.213213-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u095D\u0928\u093E \u092F\
  \u0939 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u0915\u093F \u0906\u092A \u0905\
  \u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\
  \u094B \u0936\u0941\u0930\u0942 \u0915\u0930\u0924\u0947 \u0935\u0915\u094D\u0924\
  \ \u0909\u0938\u0947 \u0905\u0924\u093F\u0930\u093F\u0915\u094D\u0924 \u091C\u093E\
  \u0928\u0915\u093E\u0930\u0940 \u0926\u0947 \u0938\u0915\u0947\u0902\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0924\u093E\u0915\u093F \u0935\u094B\u2026"
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्गुमेंट्स पढ़ना यह तरीका है कि आप अपने प्रोग्राम को शुरू करते वक्त उसे अतिरिक्त जानकारी दे सकें। प्रोग्रामर इसे इस्तेमाल करते हैं ताकि वो एप्लिकेशन को ज्यादा फ्लैक्सिबल बना सकें, और यूजर्स से इनपुट्स ले सकें।

## How to: (कैसे करें:)
ध्यान दें, Elm में डायरेक्ट कमांड लाइन आर्गुमेंट्स पढ़ना सपोर्टेड नहीं है क्योंकि यह फ्रंट-एंड वेब प्रोग्रामिंग के लिए बनाया गया है। परन्तु, अगर आप एक Elm प्रोग्राम को Node.js पर चला रहे हैं, तो आप flags की मदद से आर्गुमेंट्स पास कर सकते हैं। यहां एक उदाहरण है:

```Elm
port module Main exposing (..)

port toJs : String -> Cmd msg

main =
    Program.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init flags =
    ([], toJs flags)

update msg model =
    (model, Cmd.none)

subscriptions model =
    Sub.none
```

और फिर JavaScript में:

```javascript
const { Elm } = require('./elm.js');

const app = Elm.Main.init({
  flags: process.argv.slice(2).join(" ")
});

app.ports.toJs.subscribe(console.log);
```

सैंपल आउटपुट:

```
node main.js Hello Elm!
"Hello Elm!"
```

## Deep Dive (गहराई से समझें):
Elm में कमांड लाइन आर्गुमेंट्स डायरेक्ट पढ़ने की क्षमता नहीं है क्योंकि इसकी मुख्य फोकस वेब ब्राउजर में चलने वाले एप्लिकेशन्स पर है। जहाँ तक इतिहास की बात है, Elm ऐसे डिजाइन किया गया था कि वह प्रिडिक्टेबल वेब एप्स बनाने में मदद करे।

अल्टरनेटिव के रूप में आप Node.js सर्वर या इलेक्ट्रॉन जैसे टूल्स के साथ Elm को यूज कर सकते हैं, जहां आप 'flags' की मदद से जरूरी डाटा Elm प्रोग्राम को भेज सकते हैं। इंप्लीमेंटेशन में आमतौर पर यूज ऑफ जावास्क्रिप्ट इंटरऑप का होता है, जैसा कि ऊपर उदाहरण में दिखाया गया है।

## See Also (और भी देखें):
- Elm official guide on interop with JavaScript: [Elm Guide: Interop](https://guide.elm-lang.org/interop/)
- Elm package for server-side applications (Elm 0.19.1+): [elm-fullstack](https://github.com/elm-fullstack/elm-fullstack)
- Detailed article about Elm and Node.js: [Elm and Node.js](https://medium.com/@_rchaves_/using-elm-0-19-with-node-6f0c681e8a4c)
