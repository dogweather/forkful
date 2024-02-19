---
aliases:
- /hi/elm/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:38.717000-07:00
description: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\u093F\u0935\
  \u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u092A\u0948\u091F\
  \u0930\u094D\u0928 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u093F\u0928\
  \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0905\u0915\u094D\u0937\u0930 \u0938\
  \u0902\u092F\u094B\u091C\u0928\u094B\u0902 \u0915\u0940 \u092E\u093F\u0932\u093E\
  \u0928 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948\u0964 Elm \u092E\u0947\u0902, \u0905\u0928\u094D\u092F \u092D\
  \u093E\u0937\u093E\u0913\u0902\u2026"
lastmod: 2024-02-18 23:09:03.173590
model: gpt-4-0125-preview
summary: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\u093F\u0935\
  \u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u092A\u0948\u091F\
  \u0930\u094D\u0928 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u093F\u0928\
  \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0905\u0915\u094D\u0937\u0930 \u0938\
  \u0902\u092F\u094B\u091C\u0928\u094B\u0902 \u0915\u0940 \u092E\u093F\u0932\u093E\
  \u0928 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948\u0964 Elm \u092E\u0947\u0902, \u0905\u0928\u094D\u092F \u092D\
  \u093E\u0937\u093E\u0913\u0902\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में नियमित अभिव्यक्तियाँ (regex) पैटर्न होते हैं जिनका उपयोग स्ट्रिंग्स में अक्षर संयोजनों की मिलान के लिए किया जाता है। Elm में, अन्य भाषाओं की तरह, प्रोग्रामर इनपुट मान्य करने, खोजने, और स्ट्रिंग्स के भीतर टेक्स्ट को बदलने जैसे कार्यों के लिए regex का उपयोग करते हैं, इसकी लचीलापन और कुशलता के कारण।

## कैसे करें:
Elm की मूल लाइब्रेरी में बिल्ट-इन regex फ़ंक्शंस नहीं होते हैं, इसलिए इन कार्यों के लिए तीसरे पक्ष की लाइब्रेरियों का उपयोग करना पड़ता है। Regex के साथ काम करने के लिए लोकप्रिय विकल्पों में से एक `elm/regex` है। आप इसे `elm install elm/regex` का उपयोग करके अपनी परियोजना में जोड़ सकते हैं।

यहाँ बताया गया है कि आप कैसे `elm/regex` का उपयोग कुछ सामान्य कार्यों के लिए कर सकते हैं:

### 1. एक पैटर्न मिलान
यदि एक स्ट्रिंग एक पैटर्न से मिलती है, तो पता लगाने के लिए आप `Regex.contains` का उपयोग कर सकते हैं।

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- उदाहरण उपयोग:
isAlphanumeric "Elm2023"     -- आउटपुट: True
isAlphanumeric "Elm 2023!"   -- आउटपुट: False
```

### 2. सभी मिलान खोजना
स्ट्रिंग के भीतर एक पैटर्न के सभी अवसरों को ढूँढने के लिए, आप `Regex.find` का उपयोग कर सकते हैं।

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- उदाहरण उपयोग:
getWords "Elm is fun!"  -- आउटपुट: ["Elm", "is", "fun"]
```

### 3. टेक्स्ट बदलना
एक स्ट्रिंग के भागों को बदलने के लिए, जो एक पैटर्न से मिलते हैं, आप `Regex.replace` का उपयोग करते हैं।

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- उदाहरण उपयोग:
replaceElmWithHaskell "Learning Elm is fun!"  
-- आउटपुट: "Learning Haskell is fun!"
```

इन उदाहरणों में, `Regex.fromString` का उपयोग एक regex पैटर्न को संकलित करने के लिए किया जाता है, जहाँ `\b` शब्द सीमाओं का मिलान करता है, और `\w` किसी भी शब्द अक्षर का मिलान करता है। `Regex.fromString` के `Maybe` परिणाम को हमेशा संभालें, ताकि अवैध regex पैटर्नों के खिलाफ सुरक्षा सुनिश्चित हो सके, आमतौर पर `Maybe.withDefault` का उपयोग करके।
