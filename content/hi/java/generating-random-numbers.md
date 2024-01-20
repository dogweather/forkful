---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रैंडम नंबर उत्पन्न करना मतलब होता है अनपेक्षितापूर्वक नंबर प्राप्त करना। यह उन स्थितियों में उपयोगी होता है जब किसी अनपेक्षित परिणाम की जरुरत होती है, जैसे कि कसीनो गेम में, या किसी सिमुलेशन के दरम्यान पढ़ने के लिए डाटा चुनने के लिए। 

## कैसे:

Java में रैंडम नंबर उत्पन्न करने के लिए, हम `java.util.Random` या `java.util.concurrent.ThreadLocalRandom` क्लास का उपयोग कर सकते हैं। 

 ```Java
    import java.util.Random;

    public class Main {
        public static void main(String[] args) {
            Random rand = new Random();
            int randomNumber = rand.nextInt(100);
            System.out.println("Generated Random Number is : " + randomNumber);
        }
    }
 ```
 आउटपुट अनपेक्षित रूप से बदलेगा, लेकिन यह 0 से 99 के बीच का कोई एक नंबर होगा।

## गहरा डाइव:

रैंडम नंबर उत्पन्न करने की आवश्यकता लकड़ी से चलकर कॉम्प्युटर साइंस तक है। `java.util.Random` क्लास 1997 में जावा 1.1 के साथ जारी किया गया, जबकि `java.util.concurrent.ThreadLocalRandom` क्लास 2004 में जावा 1.5 के साथ जारी किया गया।

`java.util.Random` का उपयोग एकल थ्रेडे एनवायरनमेंट में, और `java.util.concurrent.ThreadLocalRandom` का उपयोग बहु थ्रेडे एनवायरनमेंट में होता है। जबकी `java.util.concurrent.ThreadLocalRandom` क्लास `java.util.Random` क्लास की संवर्धित श्रेणी है।

## देखने के लिए भी:

1. आधिकारिक जावा डॉक्युमेंटेशन ([`java.util.Random`](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/Random.html), [`java.util.concurrent.ThreadLocalRandom`](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html))
2. Stack Overflow : ["जावा में एक रैंडम नंबर कैसे जनरेट करें?"](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java)
3. Baeldung tutorial : ["रैंडम नंबर जनरेट करने के लिए जावा का गाइड"](https://www.baeldung.com/java-generate-random-long-float-integer-double)