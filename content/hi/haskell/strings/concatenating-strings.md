---
title:                "स्ट्रिंग को जोड़ना"
aliases: - /hi/haskell/concatenating-strings.md
date:                  2024-01-20T17:35:21.321811-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स का संयोजन (concatenation) मतलब दो या दो से ज्यादा टेक्स्ट वैल्यूज़ को जोड़ना होता है। प्रोग्रामर्स यह तब करते हैं जब उन्हें मैसेजेज तैयार करने होते हैं या डाटा को संरचित करना होता है।

## How to: (कैसे करें:)
Haskell में स्ट्रिंग्स को दो तरीकों से जोड़ सकते हैं: `++` ऑपरेटर का इस्तेमाल करके या `concat` फ़ंक्शन का प्रयोग करके।

```Haskell
-- ++ ऑपरेटर का इस्तेमाल करके
main = putStrLn ( "नमस्ते, " ++ "दुनिया!" )
```
आउटपुट:
```
नमस्ते, दुनिया!
```

लिस्ट्स के लिए:
```Haskell
-- concat फ़ंक्शन का इस्तेमाल
main = putStrLn ( concat ["नमस्ते, ", "दुनिया!", " कैसी हो?"] )
```
आउटपुट:
```
नमस्ते, दुनिया! कैसी हो?
```

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग्स को जोड़ने की प्रक्रिया भाषाओँ के इतिहास में काफी पहले से है। Haskell में स्ट्रिंग्स वास्तव में कैरेक्टर्स की लिस्ट होती हैं, इसीलिए `++` और `concat` काम करते हैं। बड़ी स्ट्रिंग्स को जोड़ते समय, परफ़ॉर्मेंस का ध्यान रखना होता है क्योंकि `++` ऑपरेटर पूरी लिस्ट को पार करता है। विकल्प के रूप में, `Data.Text` मॉड्यूल का `append` फ़ंक्शन या `Builder` मॉड्यूल का इस्तेमाल बेहतर परफ़ॉर्मेंस के लिए किया जा सकता है।

## See Also (और भी इन्फ़ॉर्मेशन):
- Haskell लिस्ट्स: [HaskellWiki Lists](https://wiki.haskell.org/Haskell)
- `Data.Text` मॉड्यूल: [Data.Text on Hackage](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- स्ट्रिंग परफ़ॉर्मेंस चर्चा: [Haskell Performance Wiki](https://wiki.haskell.org/Performance)
