---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को कोनकेटिनेट करना यानी दो या दो से अधिक स्ट्रिंग्स को एक साथ जोड़ना। कोड में मेसेज साझा करने, डाटा फॉर्मैट करने, और नई जानकारी उत्पन्न करने के लिए, प्रोग्रामर्स इसे उपयोग करते हैं।

## कैसे करें:

```Elm
import Html exposing (text)

main =
    text (String.concat ["नमस्ते, ", "दुनिया!"])
```

यह कोड "नमस्ते, " और "दुनिया!" को कोनकेटिनेट करता है, और "नमस्ते, दुनिया!" आउटपुट देता है।

## गहान जानकारी

स्ट्रिंग कोंकेनेशन की आवश्यकता और उसके प्रयोग के सावधानियां कहीं-कहीं इसे परंपरागत इसीलिए नहीं बनती हैं। Elm में `String.concat` हमें फ़्लेक्सिबिलिटी देता है कि हम कई स्ट्रिंग्स को एकत्रित कर सकते हैं। वैकल्पिक तरीके में, आप `String.append` भी उपयोग कर सकते हैं जो केवल दो स्ट्रिंग्स को मिलाता है।

## भी देखें:

1. Elm का अधिकारिक दस्तावेज़ीकरण: [String.concat](https://package.elm-lang.org/packages/elm/core/latest/String#concat) और [String.append](https://package.elm-lang.org/packages/elm/core/latest/String#append)