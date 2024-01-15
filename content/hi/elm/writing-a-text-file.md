---
title:                "एक पाठ फाइल लिखना"
html_title:           "Elm: एक पाठ फाइल लिखना"
simple_title:         "एक पाठ फाइल लिखना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी आमतौर पर दस्तावेज़ लिखने के लिए एक पाठ फ़ाइल का उपयोग नहीं करता है, लेकिन एलम के माध्यम से पाठ फ़ाइल लिखने के लिए एक शानदार और सरल तरीका है। यह आपको आपके एप्लिकेशन में स्थान से संबंधित सूचनाओं को सुरक्षित रूप से संग्रहीत करने की अनुमति देता है। 

## कैसे करें
```elm
module Main exposing (..)

import Text

main =
    Text.toFile "mytextfile.txt" "यह टेक्स्ट फ़ाइल एलम के माध्यम से लिखी गई है।"
```

Output: यह टेक्स्ट फ़ाइल एलम के माध्यम से लिखी गई है।

## गहराई में जाएं
पाठ फाइल एलम में उपलब्धकर्ता के रूप में काम करती है और इससे आप अपनी दस्तावेज़ को प्रारूपित कर सकते हैं। आप text और Text.encode फ़ंक्शन का भी उपयोग कर सकते हैं, जो आपको अलग अलग टेक्स्ट संरचनाओं को बनाने की अनुमति देते हैं।

## देखें भी
- [एलम गाइड](https://guide.elm-lang.org/)
- [एलम वेबसाइट](https://elm-lang.org/)