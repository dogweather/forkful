---
title:    "Kotlin: एक तारीख को एक स्ट्रिंग में रूपांतरण करना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोतलिन प्रोग्रामिंग भाषा का उपयोग डेटा को प्रबंधन करने और उसको अलग-अलग फॉर्मेट्स में प्रदर्शित करने के लिए किया जाता है। इसलिए, अक्सर डेटा को श्रृंखला में देखने के लिए उपयोगकर्ता उसे स्ट्रिंग में रूपांतरित करते हैं।

## कैसे करें

```Kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val currentDate = Date() // वर्तमान दिनाँक बनाएं
    val sdf = SimpleDateFormat("dd/MM/yyyy") // डेट प्रारूप में परिवर्तन करने के लिए समझदार प्रारूप
    val dateAsString = sdf.format(currentDate) // डेट को स्ट्रिंग में बदलें
    println(dateAsString) // परिणाम छापें: "26/03/2021"
}
```

## गहराई में जाइए

अब, जब आप डेट को स्ट्रिंग में बदलते हैं, कोटलिन आपको कई समझदार प्रारूप की सुविधाएं प्रदान करता है। आप यह निर्णय ले सकते हैं कि आप डेट को किस प्रारूप में तथा किस भाषा में देखना चाहते हैं। आप इसके अलावा डेट ऑब्जेक्ट पर अनेक फंक्शन भी लागू कर सकते हैं जिससे आप अपने डेटा को और भी अधिक मुक्त रूप से संभाल सकते हैं।

## देखें भी

- [कोटलिन समझदार डेट प्रारूप और भाषाओं की एक व्याख्या](https://medium.com/kotlin-conf-hunt/smart-date-formatting-and-locale-in-kotlin-d7806a6a4700) 
- [जावा डेट प्रारूप के साथ कोटलिन स्ट्रिंग परिवर्तन](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-kotlin.-time/string.html)