---
title:                "HTML विश्लेषण"
aliases:
- /hi/kotlin/parsing-html/
date:                  2024-02-03T19:13:14.090950-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग का मतलब एक वेबपेज के मार्कअप को किसी प्रोग्राम द्वारा समझे और हेरफेर कर सके ऐसे कुछ में विभाजित करना होता है। प्रोग्रामर HTML को पार्स करते हैं ताकि डाटा निकाल सकें, वेब इंटरैक्शन्स को ऑटोमेट कर सकें, या सिस्टम्स के बीच में सामग्री को माइग्रेट कर सकें।

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
