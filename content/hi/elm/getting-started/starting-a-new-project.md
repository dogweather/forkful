---
date: 2024-01-20 18:03:26.072377-07:00
description: "\u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0916\u093E\u0932\u0940 \u0915\u0948\u0928\u0935\u0938 \u092A\u0930 \u0915\
  \u094B\u0921\u093F\u0902\u0917 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0947 \u0928\u090F \u0935\u093F\u091A\u093E\u0930\u094B\u0902 \u0915\u094B \u0906\
  \u091C\u092E\u093E\u0928\u0947, \u0938\u0940\u0916\u0928\u0947 \u0914\u0930 \u0928\
  \u090F \u0938\u0949\u092B\u093C\u094D\u091F\u0935\u0947\u092F\u0930 \u092C\u0928\
  \u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:52.187786-06:00'
model: gpt-4-1106-preview
summary: "\u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0916\u093E\u0932\u0940 \u0915\u0948\u0928\u0935\u0938 \u092A\u0930 \u0915\
  \u094B\u0921\u093F\u0902\u0917 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0947 \u0928\u090F \u0935\u093F\u091A\u093E\u0930\u094B\u0902 \u0915\u094B \u0906\
  \u091C\u092E\u093E\u0928\u0947, \u0938\u0940\u0916\u0928\u0947 \u0914\u0930 \u0928\
  \u090F \u0938\u0949\u092B\u093C\u094D\u091F\u0935\u0947\u092F\u0930 \u092C\u0928\
  \u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902\u0964."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## How to: (कैसे करें:)
Elm के साथ नए प्रोजेक्ट शुरू करने के लिए:

1. Elm को इंस्टॉल करें:
   ```sh
   npm install -g elm
   ```

2. नया प्रोजेक्ट बनाएँ:
   ```sh
   elm init
   ```

3. `Main.elm` फाइल में नीचे कोड लिखें:
   ```elm
   module Main exposing (..)
   import Html exposing (text)
   
   main = 
     text "नमस्ते, Elm दुनिया!"
   ```

4. प्रोजेक्ट रन करें:
   ```sh
   elm reactor
   ```
   ब्राउज़र में `http://localhost:8000` ओपन करें और `Main.elm` फाइल को चुनें।

## Deep Dive (गहराई से जानकारी):
Elm एक फंक्शनल प्रोग्रामिंग भाषा है जो वेब ऍप्लिकेशंस को सरल और मजबूत बनाने के लिए बनी। इसकी शुरुआत 2012 में Evan Czaplicki ने की थी। Elm का मुख्य उद्देश्य है runtime errors को ना के बराबर करना। इसकी आर्किटेक्चर को Elm Architecture (TEA) कहते हैं, जो “Model-Update-View” पैटर्न पर आधारित है।

Elm प्रोजेक्ट शुरू करना आसान है और यह प्रोग्रामर को immutable data structures, static typing और friendly compiler messages प्रदान करता है। Elm के विकल्प में PureScript और ReasonML आते हैं, पर Elm की simplicity और त्रुटि-संदेशों की स्पष्टता इसे अनूठा बनाती है।

## See Also (संबंधित जानकारियां):
- [Elm Official Website](https://elm-lang.org/)
- [Elm Guide](https://guide.elm-lang.org/)
- [Elm Architecture Tutorial](https://guide.elm-lang.org/architecture/)
- [Awesome Elm: A curated list of useful Elm tutorials, libraries and software](https://github.com/sporto/awesome-elm)
