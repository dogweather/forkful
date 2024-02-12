---
title:                "सबस्ट्रिंग्स निकालना"
aliases: - /hi/kotlin/extracting-substrings.md
date:                  2024-01-20T17:46:20.446209-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

सबस्ट्रिंग्स निकालना मतलब मूल स्ट्रिंग से विशेष अंश को चुनकर उपयोग करना। प्रोग्रामर्स इसे डेटा का विश्लेषण करने, सूचना संसाधित करने, या यूजर की इनपुट को मान्यता देने के लिए करते हैं।

## कैसे करें? (How to:)

```kotlin
fun main() {
    val text = "नमस्कार, Kotlin!"

    // पहले 5 characters निकालें
    val substring1 = text.substring(0, 5)
    println(substring1) // नमस्कार

    // शब्द 'Kotlin' निकालें
    val kotlinWordIndex = text.indexOf("Kotlin")
    val substring2 = text.substring(kotlinWordIndex)
    println(substring2) // Kotlin!
}
```

## गहराई में जानकारी (Deep Dive)

सबस्ट्रिंग्स की क्षमता शुरुआती प्रोग्रामिंग भाषाओं से ही रही है - यह स्ट्रिंग मेंपुलेशन का एक बुनियादी हिस्सा है। Kotlin में `substring` फंक्शन दो प्रकार से प्रयोग किये जा सकते हैं: इंडेक्स रेंज देकर या शुरू और अंतिम इंडेक्स देकर। जावा की `String` कक्षा से Kotlin में हुआ यह सुधार ज्यादा सुगम और लचीला है। अल्टरनेटिव्स में `take`, `drop`, `filter`, `removeRange` इत्यादि फंक्शन्स शामिल हैं जो कस्टम सबस्ट्रिंग्स बनाने की अनुमति देते हैं।

## अन्य संसाधन (See Also)

- [Kotlin Documentation - Basic Types: Strings](https://kotlinlang.org/docs/basic-types.html#strings)
