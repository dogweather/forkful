---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:14.090950-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Kotlin \u092E\u0947\
  \u0902 Jsoup \u091C\u0948\u0938\u0947 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940\u091C \u0915\u0940 \u092E\u0926\u0926 \u0938\u0947 HTML \u092A\u093E\
  \u0930\u094D\u0938\u093F\u0902\u0917 \u0938\u0940\u0927\u0940 \u0939\u094B \u091C\
  \u093E\u0924\u0940 \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u092C\u0924\u093E\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948 \u0915\u093F \u0906\u092A \u092F\u0939\
  \ \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  ."
lastmod: '2024-03-13T22:44:52.255217-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902 Jsoup \u091C\u0948\u0938\u0947 \u0932\u093E\u0907\
  \u092C\u094D\u0930\u0947\u0930\u0940\u091C \u0915\u0940 \u092E\u0926\u0926 \u0938\
  \u0947 HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0938\u0940\u0927\u0940\
  \ \u0939\u094B \u091C\u093E\u0924\u0940 \u0939\u0948\u0964 \u092F\u0939\u093E\u0902\
  \ \u092C\u0924\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948 \u0915\u093F \u0906\
  \u092A \u092F\u0939 \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
Kotlin में Jsoup जैसे लाइब्रेरीज की मदद से HTML पार्सिंग सीधी हो जाती है। यहां बताया गया है कि आप यह कैसे कर सकते हैं:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Sample Page</title></head><body><p>यह एक परीक्षण है।</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Title: $title")  // परिणाम: Title: Sample Page

    val pText = doc.select("p").first()?.text()
    println("Paragraph: $pText")  // परिणाम: Paragraph: यह एक परीक्षण है।
}
```

हमने शीर्षक और पैराग्राफ टेक्स्ट लिया, जो Jsoup के सक्षम होने की सामान्य जानकारी प्रदान करता है। लेकिन यह एक शुरूआत है।

## गहराई में जाएँ:
Kotlin से पहले, इसके लिए Java मुख्यतः उपयोग होता था, अक्सर अजीब तरीके से। Jsoup ने स्क्रिप्ट को पलट दिया, jQuery जैसे दृष्टिकोण प्रदान करके। हालाँकि, HTML पार्सिंग केवल Jsoup तक ही सीमित नहीं है; HtmlUnit जैसी अन्य लाइब्रेरीज या यहाँ तक कि रेगेक्स (हालाँकि इसकी सलाह नहीं दी जाती) भी मौजूद हैं। Jsoup के साथ, आप सुनिश्चित करते हैं कि आपकी पार्सिंग दस्तावेज़ की संरचना का सम्मान करती है। यह एक DOM मॉडल का उपयोग करता है, जो तत्वों का चयन और हेरफेर को सक्षम बनाता है। यह लचीला भी है— यह सबसे अधिक गन्दे HTML को भी पार्स कर सकता है।

## देखें भी:
Jsoup में गहराई से जाने के लिए:

- Jsoup आधिकारिक दस्तावेज़ीकरण: https://jsoup.org/
- "Kotlin for Android Developers" पुस्तक: https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin प्रोग्रामिंग भाषा आधिकारिक साइट: https://kotlinlang.org/

वेब स्क्रेपिंग और पार्सिंग पर व्यापक चर्चाओं और ट्यूटोरियल्स के लिए:

- Kotlin और Jsoup के साथ वेब स्क्रेपिंग: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Kotlin और Jsoup के साथ Android पर HTML पार्सिंग: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
