---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:42.098573-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? / क्या है और क्यों?

स्ट्रिंग्स को जोड़ना (concatenating strings) का मतलब है दो या दो से ज़्यादा टेक्स्ट टुकड़ों को एक साथ चिपकाना। प्रोग्रामर्स इसे डेटा को फॉरमेट करने, संदेश दिखाने या फाइलों के पाथ बनाने के लिए करते हैं।

## How to: / कैसे करें:

Elm में स्ट्रिंग्स जोड़ते समय आप `(++)` ऑपरेटर का इस्तेमाल कर सकते हैं:

```Elm
hello : String
hello = "नमस्ते "

world : String
world = "दुनिया!"

greeting : String
greeting = hello ++ world

-- आउटपुट: "नमस्ते दुनिया!"
```

सीधा और सरल। `++` का इस्तेमाल करके आप कितने भी टेक्स्ट को जोड़ सकते हैं।

## Deep Dive / गहन जानकारी:

ऐतिहासिक दृष्टि से, स्ट्रिंग्स का संचित करना बहुत ज़रूरी रहा है। पहले के कंप्यूटर्स में मेमोरी सीमित होती थी, इसलिए कुशलता से स्ट्रिंग्स संचित करना ज़रूरी था। Elm में, स्ट्रिंग्स जोड़ते वक्त प्रदर्शन (performance) पर भी ध्यान दिया जाता है।

Elm में `(++)` ऑपरेटर बहुत ही कारगर है, लेकिन जब आपके पास बहुत सारे स्ट्रिंग्स होते हैं जो जोड़ने हैं, `String.concat` फंक्शन का इस्तेमाल करना बेहतर हो सकता है:

```Elm
parts : List String
parts = [ "यह ", "एक ", "संदेश ", "है।" ]

message : String
message = String.concat parts

-- आउटपुट: "यह एक संदेश है।"
```

इस तरीके से आप एक साथ पूरी लिस्ट को जोड़ सकते हैं। यह तरीका जटिल कोड में स्पष्टता लाने के साथ-साथ प्रदर्शन में सुधार भी करता है।

## See Also / यह भी देखें:

- Elm डॉक्यूमेंटेशन में `String` मॉड्यूल: [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)