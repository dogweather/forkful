---
title:    "Kotlin: वर्तमान तारीख प्राप्त करना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

अभी की तारीख जानने के लिए किसी ने क्यों संलग्न होना चाहिए। यह करने का कारण है कि यह प्रोग्रामिंग का एक महत्वपूर्ण हिस्सा है। 

## कैसे करना है

डेटा एंड टाइम को देखने के लिए, आपको कुछ संगठन और सूचनाओं का मुकाबला करना होगा। यहां हम कुछ उदाहरण प्रस्तुत कर रहे हैं जो Kotlin में तारीख का उपयोग करते हैं।

उदाहरण 1: वर्तमान तारीख के साथ एक स्ट्रिंग मिलाना (Concatenate)

```kotlin
val currentDate = LocalDate.now()

// Output: 14 नवंबर 2021
println("${currentDate.dayOfMonth} ${currentDate.month} ${currentDate.year}")
```

उदाहरण 2: वर्तमान तारीख और समय दिखाएं (Show current date and time)

```kotlin
val currentDateTime = LocalDateTime.now()

// Output: 14-11-2021 05:30 PM
println("$currentDateTime.toLocalDate()} ${currentDateTime.hour}:${currentDateTime.minute toLong()} ${currentDateTime.toLocalTime}")
```

उदाहरण 3: प्रत्येक महीने की संख्या दिखाएं (Show the number of days in each month)

```kotlin
val currentYear = Year.now()
val months = Month.values()

for (month in months) {
    val days = month.length(currentYear isLeap)
    
    // Output: नवंबर के देश में 30 दिन होते हैं
    println("${month} में ${days} दिन होते हैं")
}
```

## गहराई में जाएं

वर्तमान तारीख आपके प्रोग्राम के लिए आवश्यक होने के साथ-साथ अन्य उपयोगिता के लिए भी महत्वपूर्ण है। Kotlin में, इसके लिए कई इनबिल्ट फंक्शन के साथ आप दिन, महीने, साल, और अन्य कैलेंडरिक इकाइयों से जुड़ी जानकरी प्राप्त कर सकते हैं। आप यह भी संभव होगा कि आप अपने आवश्यकताओं के लिए अपने तारीख प्रस्तुतव्य प्रारूप आयोजित करें।

## देखें भी

- [कोट्लिन डॉक्यूम