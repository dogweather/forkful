---
title:    "Kotlin: टेक्स्ट खोज और प्रतिस्थापन"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##वजह

कैसे कोई लोग टेक्स्ट को तलाश करने और बदलने में रुचि रखते हैं के बारे में केवल 1-2 वाक्यों में समझाने की बात कर सकते हैं। 

##कैसे करें

```Kotlin
fun main() {
    var text = "मेरा नाम जॉन है"
    println(text.replace("जॉन", "केविन"))
    
    //Output: मेरा नाम केविन है
}
```
 

##गहराई में जाएं

टेक्स्ट को तलाश करने और बदलने का काम कहीं नहीं दिखता हैं, लेकिन यह बहुत ही उपयोगी है। आप अपने टेक्स्ट को और अधिक असरदार बनाने के लिए बहुत सारे विकल्पों को जांच सकते हैं। आप चरणों को अधिक सीमित बनाकर, एक ही चरण में बदलाव कर सकते हैं, या योग्यता अनुसार टेक्स्ट को तलाश सकते हैं।

##इस प्रकार देखें

यह काम आपके कोटलिन प्रोजेक्ट को अधिक उपयोगी और उपयोगकर्ता अनुकूल बनाने में मदद कर सकता है। और अधिक जानने के लिए, आप कुछ और उपयोगी लिंकों को जांच सकते हैं: 

[कोटलिन स्ट्रिंग फंक्शन सहज रूप से समझाया](https://kotlinlang.org/docs/reference/strings-overview.html)

[गाइड: टेक्स्ट त्याग करें](https://proandroiddev.com/kotlin-how-to-remove-characters-and-explain-the-different-kinds-of-text-stripping-de31980a22e3)

[कोटलिन में वर्णनात्मक बदलाव का उपयोग करके टेक्स्ट बदलें](https://www.bignerdranch.com/blog/replace-text-effectively-in-kotlin-using-descriptive-transformations/)