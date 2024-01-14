---
title:                "Kotlin: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Hindi में ब्लॉग पोस्ट कॉटलिन प्रोग्रामिंग के लिए

## क्यों:

कोई भी इंसान तारीख को स्ट्रिंग में बदलने करने से पहले, इसका क्यों करना चाहेंगे।

## कैसे करें:

```Kotlin

fun main() {
    // तारीख को स्ट्रिंग में बदलने का उदाहरण
    val date = LocalDate.of(2020, 10, 31)
    println("तारीख: $date")
    val dateString = date.toString()
    println("स्ट्रिंग: $dateString")
}

```

आउटपुट: 
तारीख: 2020-10-31
स्ट्रिंग: 2020-10-31

## गहराई में जाएं:

बहुत सारे समस्याओं को हल करने के लिए हमारे पास तारीखों को अलग-अलग स्थानों पर उपयोग करने की आवश्यकता हो सकती है। इस ब्लॉग अप और `LocalDate` और `toString()` का उपयोग करके हम तारीख को स्ट्रिंग में बदलने का कार्य को स्पष्ट करेंगे।

## इस से जुड़ी और भी पढ़ें:

- Kotlin में डेट और टाइम: https://www.journaldev.com/27570/kotlin-datetime
- इंसान डेट को स्ट्रिंग में बदलना: https://www.educative.io/edpresso/how-to-convert-a-string-to-date-in-kotlin
- एक संपूर्ण तारीख और समय लाइब्रेरी: https://github.com/Karumi/Kalendar