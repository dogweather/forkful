---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:03:50.578491-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना मतलब बिल्कुल नया कोड बेस तैयार करना, जैसे खाली कैनवास पर पेंटिंग करना। प्रोग्रामर इसे तब करते हैं जब उन्हें नई वेबसाइट, एप्लिकेशन या सिस्टम बनाना है या कोई नई प्रोब्लेम सोल्व करनी होती है।

## How to: (कैसे करें:)
यहाँ एक साधारण Java प्रोजेक्ट शुरू करने का उदाहरण दिया गया है:

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

इस कोड को रन करने पर आपको निम्नलिखित आउटपुट देखने मिलेगा:

```
Hello, World!
```

## Deep Dive (गहराई से जानकारी)
प्राचीन समय से जब लोग मैनुअली कोड टाइप करते थे, तब से अब जब IDEs जैसे IntelliJ IDEA और Eclipse ने कोडिंग को और भी ज्यादा सरल बनाया है, प्रोजेक्ट शुरू करने के तरीके बहुत बदल चुके हैं। पहले जहां सिर्फ टेक्स्ट एडिटर और कमांड लाइन हुआ करती थी, वहीं अब Maven और Gradle जैसे बिल्ड टूल्स ने डिपेंडेंसी मैनेजमेंट और बिल्ड प्रोसेस को काफी आसान बना दिया है। हालांकि Java के सबसे बेसिक फॉर्म में, जैसे ऊपर दिखाया गया कोड सैंपल में, आपको सिर्फ `public static void main(String[] args)` मेथड की ज़रूरत होती है जिसे जावा एप्लिकेशन की शुरुवात माना जाता है।

## See Also (और जानकारी)
- Oracle Java Documentation:

  [Official Java Tutorials](https://docs.oracle.com/javase/tutorial/)

- Apache Maven और Gradle के बारे में ज्यादा जानें:

  [Maven Getting Started Guide](https://maven.apache.org/guides/getting-started/)

  [Gradle Guides](https://gradle.org/guides/) 

- अन्य उपयोगी IDEs:

  [IntelliJ IDEA](https://www.jetbrains.com/idea/)

  [Eclipse](https://www.eclipse.org/)
