---
title:                "Kotlin: पैटर्न से मेल खाते अक्षर हटाना"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी क्यों चुनेगा कि वे किसी भी पैटर्न से मैच करने वाले अक्षरों को हटाएं?

## कैसे करें

```Kotlin
fun deleteMatchingCharacters(input: String, pattern: String): String{
    return input.replace(Regex(pattern), "")
}

println(deleteMatchingCharacters("हेल्लो वर्ल्ड", "[ाीूृेोौ्]"))
// Output: हेल वर्ल्ड

println(deleteMatchingCharacters("कोट्स", "[िुेाोूृ]") )
// Output: कोट्
```

मुख्य आकृति उपयोग करके एक नई स्ट्रिंग वापस दुबारा निर्माण करे। इसके लिए, हम `replace()` फंक्शन का उपयोग करते हैं और उस नयी दिन्र के लिये स्ट्रिंग को गुजारने वाले विशिष्ट पैटर्न प्रति सी छन्द पैर स्थानांतरण करते हैं।

## गहराई में जाएं

कभी-कभी हमारे पास संख्याओं और मूल्यों की भारी संख्या रखीत ताजगी का साथ भी होता है। स्ट्रिंग संग्रहण में जो भी स्थान में होगा आसानी से उन में से निकल जाएगा जो कम या अधिक उपयुलंबित होगा। इसलिए, अधिक विवेकपूर्ण एप्लिकेशन के साथ मालूम हो सकता है कि हम कैसे किसी मान है उसके बादों से भ्रष्ट कर सकते हैं। इसलिए, पैटर्न से मेल न करने वाले अक्षरों को हटाते हैं हम स्पष्ट माटडौन निर्देशिका द्वारा पैटर्न साभान्ती निरुपण करेंगे।

## इसके अलावा

* [Kotlin रिपो जेरासी ](https://newbedev.com/how-to-remove-special-character-from-string-using-jersey)
* [श्रृति में छिपा करना होगा](http://www.learn4future.com/replace-all-characters-in-a-string-that-match-a-pattern-in-kotlin/)