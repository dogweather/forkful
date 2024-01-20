---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:37.228376-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"क्या और क्यों?"

तारीख को स्ट्रिंग से पार्स करना मतलब स्ट्रिंग में लिखी तारीख को पढ़कर उसे किसी विशेष डेटा फॉर्मेट में बदलना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि अलग-अलग सिस्टम्स तारीख के डेटा को अलग फॉर्मेट में उपयोग और संग्रहित करते हैं।

## How to:
"कैसे करें:"

Haskell में `Data.Time` लाइब्रेरी का उपयोग करके आसानी से तारीख को पार्स कर सकते हैं। नीचे उदाहरण दिया गया है:

```Haskell
import Data.Time

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

main :: IO ()
main = do
  let dateString = "2023-03-28"
  print $ parseDate dateString
```

यह कोड निम्न आउटपुट देगा:

```
Just 2023-03-28
```

## Deep Dive:
"गहराई से जानकारी:"

तारीख को पार्स करने की आवश्यकता पुराने समय से है, जब से कंप्यूटर्स में तारीख और समय के प्रबंधन की जरूरत पड़ी। `Data.Time` हास्केल की मानक लाइब्रेरी है, पर `time` पैकेज के अलावा भी `chronos`, `thyme` जैसे विकल्प मौजूद हैं। इनपुट स्ट्रिंग को पार्स करने के लिए `parseTimeM` फंक्शन `True` फ्लैग के साथ मोनैडिक कंटेक्स्ट में सेफली पार्स करता है, जिससे गलत फॉर्मेट होने पर `Nothing` रिटर्न होता है। 

फंक्शन के अंदर `"%Y-%m-%d"` एक डेट फॉर्मेट है, जो साल-महीना-दिन का प्रतिनिधित्व करता है। इसका उपयोग करके यह धारणा की जाती है कि इनपुट स्ट्रिंग इसी फॉर्मेट में है।

## See Also:
"और देखें:"

- Haskell `time` library documentation: https://hackage.haskell.org/package/time
- Haskell Date and Time tutorial: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/date-and-time
- Alternative libraries for date and time in Haskell: 
  - https://hackage.haskell.org/package/chronos
  - https://hackage.haskell.org/package/thyme