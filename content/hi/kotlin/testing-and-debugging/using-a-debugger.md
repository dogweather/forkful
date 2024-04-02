---
date: 2024-01-26 04:09:40.205281-07:00
description: "\u092F\u0939\u093E\u0901 IntelliJ IDEA \u0915\u0947 \u0938\u093E\u0925\
  \ Kotlin \u092E\u0947\u0902 \u0921\u0940\u092C\u0917\u093F\u0902\u0917 \u0915\u093E\
  \ \u091B\u094B\u091F\u093E-\u0938\u093E \u0938\u094D\u0935\u093E\u0926 \u0939\u0948\
  \ - \u0906\u0908\u0921\u0940\u0908 \u0915\u093E \u0936\u0930\u094D\u0932\u0915 \u0939\
  \u094B\u092E\u094D\u0938: ```kotlin fun main() { val mysteryNumber = 42 var guess\
  \ = 0 while\u2026"
lastmod: '2024-03-13T22:44:52.266544-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 IntelliJ IDEA \u0915\u0947 \u0938\u093E\u0925 Kotlin\
  \ \u092E\u0947\u0902 \u0921\u0940\u092C\u0917\u093F\u0902\u0917 \u0915\u093E \u091B\
  \u094B\u091F\u093E-\u0938\u093E \u0938\u094D\u0935\u093E\u0926 \u0939\u0948 - \u0906\
  \u0908\u0921\u0940\u0908 \u0915\u093E \u0936\u0930\u094D\u0932\u0915 \u0939\u094B\
  \u092E\u094D\u0938: ```kotlin fun main() { val mysteryNumber = 42 var guess = 0\
  \ while\u2026"
title: "\u0921\u0940\u092C\u0917\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E"
weight: 35
---

## कैसे:
यहाँ IntelliJ IDEA के साथ Kotlin में डीबगिंग का छोटा-सा स्वाद है - आईडीई का शर्लक होम्स:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Guess the number: ")
        guess = readLine()?.toIntOrNull() ?: continue // खराब इनपुट्स को नज़रअंदाज करें

        // इसे 'guess' को कार्रवाई में देखने के लिए एक ब्रेकपॉइंट सेट करें
        if (guess < mysteryNumber) {
            println("Too low!")
        } else if (guess > mysteryNumber) {
            println("Too high!")
        }
    }

    println("You've got it! The mystery number was $mysteryNumber")
}
```

डीबगर आउटपुट:
```
Guess the number: 
10
Too low!
Guess the number: 
50
Too high!
Guess the number: 
42
You've got it! The mystery number was 42
```

## गहराई में जानकारी
डीबगर '50 के दशक से खेल में रहे हैं। उस समय, वे काफी आदिम थे, और डीबगिंग सॉफ्टवेयर की तुलना में हार्डवेयर के बारे में अधिक हो सकती थी। आजकल, IntelliJ IDEA में एक डीबगर हमें कोड लाइन दर लाइन के माध्यम से चलने, वेरिएबल्स की स्थिति की जांच करने, और हमारी सुविधा के अनुसार ब्रेकपॉइंट्स सेट करने देता है।

जबकि IntelliJ का डीबगर Kotlin के लिए बहुत सुविधाजनक है, यह समुद्र में एकमात्र मछली नहीं है। Android विकास के लिए Logcat जैसे विकल्पों की एक श्रेणी है, या मिनिमलिस्ट्स के लिए जैसे कि jdb जैसे कमांड-लाइन टूल्स हैं। यहाँ का अंदरूनी जादू ज्यादातर JVM टूल इंटरफ़ेस (JVMTI) के बारे में है, जो डीबगर्स को जावा वर्चुअल मशीन के साथ बातचीत करने देता है, Kotlin डेवलपर्स को लूप में रखता है।

## देखें भी
- IntelliJ IDEA डीबगर डॉक्यूमेंटेशन: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
