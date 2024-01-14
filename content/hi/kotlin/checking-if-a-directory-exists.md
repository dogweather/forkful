---
title:                "Kotlin: डायरेक्ट्री का अस्तित्व जाँचना"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

एक निर्देशिका अस्तित्व की जाँच करने का कारण है कि हम इसे उपयोग करके अपने कोड में गलतियों को जांच सकते हैं और सुनिश्चित कर सकते हैं कि हमारे एप्लिकेशन में सही संरचना मौजूद है। 

## कैसे करें

```Kotlin
val directory = File("path/to/directory")

if (directory.exists()) {
    println("Directory exists!")
} else {
    println("Directory does not exist!")
}
```

उपरोक्त कोड से हम देख सकते हैं कि हमने अस्तित्व नि:शुल्क विधि का उपयोग किया है और उसके परिणाम को देखने के लिए एप्लिकेशन प्रविष्ट किया है। यह उपयोगकर्ता को भी पता चलता है कि क्या एक निर्देशिका मौजूद है या नहीं। 

## गहराई में जाएं

एक निर्देशिका के अस्तित्व की जाँच करने के लिए, हम इसका उपयोग अपने कोड में आसानी से कर सकते हैं और सुनिश्चित कर सकते हैं कि हमारे कोड में पहले से ही कोई अस्तित्व है या नहीं। साथ ही, इससे हमें उस निर्देशिका का पता चल जाता है जिसके अस्तित्व पर हमारे कोड का निर्भरता हो सकता है। 

## इसके अलावा देखें

- [Kotlin आधिकारिक वेबसाइट] (https://kotlinlang.org/docs/home.html)
- [JavaTpoint के लिए कोटलिन ट्यूटोरियल] (https://www.javatpoint.com/kotlin-tutorial)
- [देवेंद्र राजपूत के लिए कोटलिन सीखो] (https://dev.to/devkode19/learn-kotlin-for-android-development-a-step-by-step-guide-5gfc)