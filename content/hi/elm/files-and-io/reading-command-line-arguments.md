---
date: 2024-01-20 17:56:07.654721-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0927\u094D\
  \u092F\u093E\u0928 \u0926\u0947\u0902, Elm \u092E\u0947\u0902 \u0921\u093E\u092F\
  \u0930\u0947\u0915\u094D\u091F \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\
  \u0928 \u0906\u0930\u094D\u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\
  \u0922\u093C\u0928\u093E \u0938\u092A\u094B\u0930\u094D\u091F\u0947\u0921 \u0928\
  \u0939\u0940\u0902 \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\
  \u0939 \u092B\u094D\u0930\u0902\u091F-\u090F\u0902\u0921 \u0935\u0947\u092C \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0915\u0947\
  \ \u0932\u093F\u090F \u092C\u0928\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\
  \u0964\u2026"
lastmod: '2024-04-05T21:53:54.220401-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0927\u094D\u092F\u093E\
  \u0928 \u0926\u0947\u0902, Elm \u092E\u0947\u0902 \u0921\u093E\u092F\u0930\u0947\
  \u0915\u094D\u091F \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\
  \u0930\u094D\u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\
  \u093E \u0938\u092A\u094B\u0930\u094D\u091F\u0947\u0921 \u0928\u0939\u0940\u0902\
  \ \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u092B\u094D\
  \u0930\u0902\u091F-\u090F\u0902\u0921 \u0935\u0947\u092C \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092C\u0928\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\u0964 \u092A\u0930\
  \u0928\u094D\u0924\u0941, \u0905\u0917\u0930 \u0906\u092A \u090F\u0915 Elm \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B Node.js \u092A\u0930\
  \ \u091A\u0932\u093E \u0930\u0939\u0947 \u0939\u0948\u0902, \u0924\u094B \u0906\u092A\
  \ flags \u0915\u0940 \u092E\u0926\u0926 \u0938\u0947 \u0906\u0930\u094D\u0917\u0941\
  \u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u093E\u0938 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0902 \u090F\u0915 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

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
