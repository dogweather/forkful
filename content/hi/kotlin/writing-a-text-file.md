---
title:                "एक टेक्स्ट फाइल लिखना"
html_title:           "Kotlin: एक टेक्स्ट फाइल लिखना"
simple_title:         "एक टेक्स्ट फाइल लिखना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

कॉटलिन में एक टेक्स्ट फ़ाइल लिखने का अनुभव मनोरंजन और शैली के साथ सीखने का एक बहुत ही अच्छा तरीका है। यह एक साधारण और स्थिर ढंग से बातों को साझा करने का बहुत ही असरदार तरीका है।

## कैसे करें

टेक्स्ट फ़ाइल लिखने के लिए, सबसे पहले हम एक ```FileWriter``` ऑब्जेक्ट बनाएंगे और उसे किसी नाम से इनिशियलाइज़ करेंगे। फिर हम अपनी फ़ाइल में लिखने के लिए ```write``` फ़ंक्शन का उपयोग करेंगे और इसे कोई स्ट्रिंग पास करेंगे। अंत में, हम अपनी फ़ाइल को सहेजने के लिए ```close``` फ़ंक्शन का उपयोग करेंगे। नीचे एक संपूर्ण उदाहरण है:

```Kotlin
fun main() {
    val fileWriter = FileWriter("myfile.txt")
    fileWriter.write("Hello, World!")
    fileWriter.close()
}
```

उपरोक्त कोड का नतीजा निम्नलिखित तरीके से होगा:

```shell
> cat myfile.txt
Hello, World!
```

## गहराई में जाएं

गहराई में जाने से पहले, हमें टेक्स्ट फ़ाइल को सहेजने के अलावा भी कई और उपयोगी काम कर सकते हैं। तत्पर, हम तीन अलग-अलग तरीकों से एक फ़ाइल में लिखने का प्रयास करेंगे। ये तीन तरीके हैं:

- ```printWriter``` अस्थायी बफ़र के साथ लिखने करने के लिए होता है।
- ```bufferedWriter``` एक बफ़र में पूरी तरह से लिखने के लिए होता है।
- ```fileWriter``` सीधे फ़ाइल में लिखने के लिए होता है।

इन तीनों तरीकों को नीचे दिए गए कोड के साथ देखें:

```Kotlin
fun main() {
    // PrintWriter
    val printWriter = PrintWriter(FileWriter("myfile.txt