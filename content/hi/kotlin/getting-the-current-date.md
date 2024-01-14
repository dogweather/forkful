---
title:                "Kotlin: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप भी कभी प्रोग्रामिंग करते हुए अपने को समय का पता नहीं लगा पाते हैं? आजकल हमें अपनी दैनिक कार्यों के साथ-साथ प्रोग्रामिंग भी करनी पड़ती है तो हमारे लिए स्वयं के समय का पता होना बहुत जरूरी है। लेकिन क्या आप जानते हैं कि कोटलिन में वर्तमान तारीख मिलाना कितना आसान है? आइए इस ब्लॉग पोस्ट के माध्यम से जाने कि आप कोटलिन में वर्तमान तारीख कैसे बना सकते हैं।

## कैसे करें

वर्तमान तारीख का पता लगाना कोटलिन में बहुत आसान है। सबसे पहले, आपको `java.util` पैकेज के `Date` और `Calendar` क्लास को इम्पोर्ट करना होगा। फिर, `Calendar.getInstance()` का उपयोग करके आप कैलेंडर के एक इंस्टेंस को प्राप्त कर सकते हैं। अब, `Calendar` ऑब्जेक्ट की `get()` फंक्शन में पैरामीटर के रूप में `Calendar.YEAR`, `Calendar.MONTH` और `Calendar.DAY_OF_MONTH` जैसे फिल्ड देकर आप वर्तमान तारीख के साथ भिन्न भिन्न जानकारियां पा सकते हैं। कैसे कोड का उदाहरण देखते हैं:

```Kotlin
import java.util.Calendar
import java.util.Date

fun main() {
    val currentDate = Calendar.getInstance()

    val year = currentDate.get(Calendar.YEAR)
    val month = currentDate.get(Calendar.MONTH)
    val dayOfMonth = currentDate.get(Calendar.DAY_OF_MONTH)

    println("आज की तारीख है: $dayOfMonth/$month/$year")
}
```

इस कोड का आउटपुट निम्न होगा:

```
आज की तारीख है: 9/5/2021
```

अगर आप अपनी वर्तमान तारीख से संबंधित अन्य जानकारियां जैसे अब दिन, दिन के नाम, सप्ताह के किस दिन है आदि प्राप्त करना चाहते हैं तो आप `Date`