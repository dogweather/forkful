---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:03:26.072377-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना मतलब है खाली कैनवस पर कोडिंग शुरू करना। प्रोग्रामर इसे नए विचारों को आजमाने, सीखने और नए सॉफ़्टवेयर बनाने के लिए करते हैं।

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
