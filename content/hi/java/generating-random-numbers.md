---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:45.691011-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स अप्रत्याशित संख्याएं होती हैं। प्रोग्रामर्स टेस्टिंग, गेमिंग, सुरक्षा, और साइंस सिमुलेशंस में इन्हें उपयोग करते हैं। 

## How to: (कैसे करें:)
Java में `Random` क्लास की मदद से हम आसानी से रैंडम नंबर्स जनरेट कर सकते हैं:

```java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        // Random ऑब्जेक्ट क्रिएट करें
        Random random = new Random();

        // 0 से 100 के बीच एक रैंडम नंबर जनरेट करें
        int rndInt = random.nextInt(101); 
        System.out.println("रैंडम इंटीजर: " + rndInt);

        // एक रैंडम डबल जनरेट करें
        double rndDouble = random.nextDouble();
        System.out.println("रैंडम डबल: " + rndDouble);
    }
}
```
सैम्पल आउटपुट हर बार अलग होगा:

```
रैंडम इंटीजर: 43
रैंडम डबल: 0.5392075399730739
```

## Deep Dive (गहराई से जानकारी):
रैंडम नंबर जेनरेशन का इतिहास प्राचीन काल से है—चौसर से लेकर आधुनिक कंप्यूटर तक। Java में, `Random` क्लास के अलावा, `Math.random()`, `ThreadLocalRandom`, और Java 8 में आया `SecureRandom` जैसे विकल्प उपलब्ध हैं, जो क्रिप्टोग्राफिक रूप से सुरक्षित रैंडम नंबर प्रदान करते हैं। `Random` क्लास एक लीनियर कंजनर्टिव जेनरेटर (LCG) पर आधारित है, जो पूर्णतया रैंडम नहीं लेकिन प्रैक्टिकल पर्पज के लिए पर्याप्त है।

## See Also (अधिक जानकारी के लिए):
- Java प्रलेखन की Random क्लास: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- `Math.random()` मेथड का उपयोग: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random()
- SecureRandom क्लास के बारे में जानकारी: https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html
- `ThreadLocalRandom` क्लास: https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html
