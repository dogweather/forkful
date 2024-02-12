---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases: - /hi/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:06:21.157732-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामिंग में एक स्ट्रिंग को कैपिटलाइज करना यदि वह पहले से नहीं है, तो स्ट्रिंग के पहले अक्षर को अपरकेस में बदलना शामिल है, जो उपयोगकर्ता इनपुट्स को फॉर्मैट करने या उपयोगकर्ता इंटरफ़ेस में टेक्स्ट को एक अधिक मानकीकृत या मानव-मित्रतापूर्ण तरीके से प्रदर्शित करने में उपयोगी है। प्रोग्रामर अपने सॉफ्टवेयर एप्लिकेशन्स के भीतर डाटा संगतता सुनिश्चित करने या विशिष्ट फॉर्मैटिंग आवश्यकताओं को पूरा करने के लिए यह कार्य करते हैं।

## कैसे:

कोटलिन में, स्ट्रिंग्स को मानक पुस्तकालय कार्यों का उपयोग करके कैपिटलाइज किया जा सकता है, इसके लिए तृतीय-पक्ष लाइब्रेरीज की आवश्यकता नहीं होती। कोटलिन का स्ट्रिंग्स को संभालने का दृष्टिकोण इन कार्यों को सरल और संक्षिप्त बनाता है।

### पूरी स्ट्रिंग को कैपिटलाइज करना:

```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // प्रिंट होगा: HELLO, WORLD!
```

### केवल पहले अक्षर को कैपिटलाइज करना:

कोटलिन 1.5 के रूप में, `capitalize()` कार्य को हटा दिया गया है और इसे `replaceFirstChar` और एक लैम्बडा के संयोजन से बदल दिया गया है जो जांचता है कि क्या यह एक निम्नकेस अक्षर है जिसे अपरकेस में बदलना है।

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // प्रिंट होगा: Hello, world!
```

यह दृष्टिकोण पूरे वाक्य को इसके मूल रूप में ही रखता है जबकि केवल पहले अक्षर को अपरकेस में बदलता है।
