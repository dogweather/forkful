---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग को capitalize करना मतलब उसके पहले अक्षर को बड़े (uppercase) में बदलना। इसे प्रोग्रामर अक्सर यूजर इंटरफेस में सही ढंग से नाम और खिताब दिखाने के लिए करते हैं।

## कैसे करें:
```kotlin
fun capitalizeFirstChar(string: String): String {
    if (string.isEmpty()) return ""
    return string.first().uppercase() + string.substring(1)
}

fun main() {
    val result = capitalizeFirstChar("कोटलिन शानदार है!")
    println(result) // Output: "कोटलिन शानदार है!"
}
```

## गहराई से समझाईये:
पहले, Kotlin में `.capitalize()` फंक्शन होता था जो स्ट्रिंग के पहले अक्षर को एक शॉर्टकट के जरिए बड़ा बना देता था। पर, यह धीरे-धीरे हटा दिया गया क्योंकि यह लोकलाइजेशन के मुद्दों को उठा सकता था, जैसे कि गैर-Latin लिपियों में।

जब हम इस कार्य के लिए कस्टम फंक्शन बनाते हैं, तो हम खुद को यह सुनिश्चित करने की आजादी देते हैं कि यह बैकग्राउंड में कैसे व्यवहार करेगा।

`.uppercase()` Kotlin 1.5 में जोड़ा गया था और यह किसी भी अक्षर को उसके uppercase वर्शन में बदल देता है, इसे सही तरीके से हैंडल करते हुए कि परिणाम लोकलाइज हो।

## देखें भी:
- Kotlin String Documentation: [Kotlin String Functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Kotlin Standard Library Reference: [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/)
- Unicode Standard for Case Operations: [Unicode Standard](http://www.unicode.org/versions/Unicode13.0.0/)
