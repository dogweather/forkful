---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Kotlin: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे कोलेज, काम या सामाजिक गतिविधियों में हमें अपनी कार्यवाही को तारीख के साथ दर्ज करना होता है। उस समय हमें वर्तमान तारीख को जानने की आवश्यकता होती है। इस लेख में हम जानेंगे कि कैसे कोटलिन में वर्तमान तारीख को प्राप्त किया जा सकता है।

## कैसे करें

```Kotlin
// वर्तमान तारीख को प्राप्त करने के लिए आपको java.util.Date कक्ष का उपयोग करना होगा।
val currentDate = Date()

// इसके अलावा आप java.time.LocalDate और java.time.LocalDateTime कक्ष का भी उपयोग कर सकते हैं। ये कक्ष जावा 8 में आए हुए हैं।
val localDate = LocalDate.now()
val localDateTime = LocalDateTime.now()

// अगर आपको समय स्थान के साथ वर्तमान तारीख चाहिए, तो आप java.util.Calendar कक्ष का प्रयोग कर सकते हैं।
val calendar = Calendar.getInstance()
val today = calendar.time

// आप वर्तमान तारीख का प्रारूप भी बदल सकते हैं। चाहे आप दिनांक, महीना या वर्ष दिखाना चाहते हों, सभी संभव है। नीचे दिए गए मामलों को देखें:
// मात्रिक संख्या के साथ दिनांक
val dayOfMonth = SimpleDateFormat("dd").format(currentDate)
println("आज का दिनांक है: $dayOfMonth")

// पूर्ण दिनांक
val completeDate = SimpleDateFormat("dd MMMM yyyy").format(currentDate)
println("आज का दिनांक है: $completeDate")

// इसी तरह आप माह या वर्ष को भी दर्शा सकते हैं, आपके कोड के अनुसार।

// अगर आप वर्तमान समय को भी प्राप्त करना चाहते हैं, तो आप सरल रूप से इस्तेमाल कर सकते हैं:
val currentTime = LocalDateTime.now().toLocalTime()
println("अभी समय है: $currentTime")
```

उपरोक्त कोड ब्लॉक को संपादित करके आप वर्तमान तारीख क