---
title:                "स्ट्रिंग को निचे केस में बदलना"
html_title:           "Kotlin: स्ट्रिंग को निचे केस में बदलना"
simple_title:         "स्ट्रिंग को निचे केस में बदलना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोटलिन में स्ट्रिंग को लोअर केस में बदलने का मतलब है कि हम स्ट्रिंग के सभी अक्षरों को छोटे अक्षरों में बदलते हैं। यह प्रोग्रामर्स किसी भी स्ट्रिंग को केस सेंसिटिविटी के साथ हंडल करने के लिए करते हैं।

## कैसे:
```Kotlin
fun main(){
  val str1 = "Hello, Kotlin!"
  val str2 = "WORLD!"
  
  println(str1.toLowerCase())  // output: hello, kotlin!
  println(str2.toLowerCase())  // output: world!
}
```

## गहराई में जाएं:
कई प्रोग्रामिंग भाषाओं में स्ट्रिंग का केस बदलने के लिए फ़ंक्शन या मेथोड उपलब्ध हैं। कोटलिन में, ```toLowerCase()``` निर्दिष्ट फ़ंक्शन केस सेंसिटिविटी को मध्यम रूप से हाथियाने के लिए है। इसके अलावा, आप स्ट्रिंग को ```toUpperCase()``` फ़ंक्शन के माध्यम से अपर केस में बदल सकते हैं। स्ट्रिंग पर फ़ंक्शन के इन दो आदेशों को एक साथ लगाकर आप इसे पूरी तरह से केस सेंसिटिविटी से बचा सकते हैं।

## इसके अलावा देखें:
कोटलिन रिफ़रेंस आपको स्ट्रिंग को लोअर केस और अपर केस में बदलने के लिए अन्य विकल्पों के बारे में जानने की सुविधा उपलब्ध कराता है। आप इनका अध्ययन करके अपने कोड को और बेहतर बना सकते हैं। यहां अन्य कोडिंग संस्कप्तों में स्ट्रिंग को लोअर केस और अपर केस में बदलने का निर्धारित करने के तरीके दिए गए हैं: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)