---
title:                "डीबगर का उपयोग करना"
aliases:
- /hi/kotlin/using-a-debugger/
date:                  2024-01-26T04:09:40.205281-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डीबगर में गहराई से उतरना सभी अपने कोड में कदम दर कदम चलना, गियर्स को चलते हुए देखना और उन चिपकने वाले बग्स को लाल हाथों में पकड़ना है। प्रोग्रामर डीबगर्स का उपयोग करते हैं क्योंकि ये वह जासूसी उपकरण हैं जो हमें इस बात का पता लगाने में मदद करते हैं कि चीजें कहाँ गलत हो रही हैं बिना हमारे बाल नोचने के।

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
