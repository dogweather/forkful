---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
aliases:
- hi/java/generating-random-numbers.md
date:                  2024-01-27T20:35:45.590451-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

यादृच्छिक संख्याओं का उत्पादन अप्रत्याशित अनुक्रमों या एक निर्धारित रेंज के भीतर एकल मूल्यों के बारे में है। प्रोग्रामर इस तकनीक को विभिन्न कारणों से उपयोग करते हैं, जिनमें सिमुलेशन, खेल, सुरक्षा अनुप्रयोग और विभिन्न स्थितियों के तहत एल्गोरिदम की परीक्षा के लिए नमूना विधियाँ शामिल हैं।

## कैसे:

जावा में, `java.util` पैकेज से `Random` क्लास का उपयोग करके, या विशिष्ट उपयोग के मामलों के लिए `ThreadLocalRandom` और `SecureRandom` क्लासों का उपयोग करके यादृच्छिक संख्याएँ उत्पन्न की जा सकती हैं। निम्नलिखित उदाहरण इन क्लासों का उपयोग करने का तरीका दर्शाते हैं।

### `Random` क्लास का उपयोग करना
`Random` क्लास सरल प्रति-यादृच्छिक संख्याओं को उत्पन्न करने का एक तरीका प्रदान करती है।

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // एक Random ऑब्जेक्ट बनाएं

        int randInt = rand.nextInt(50); // 0 से 49 तक एक यादृच्छिक पूर्णांक उत्पन्न करता है
        double randDouble = rand.nextDouble(); // 0.0 और 1.0 के बीच एक यादृच्छिक डबल उत्पन्न करता है
        boolean randBoolean = rand.nextBoolean(); // एक यादृच्छिक बूलियन उत्पन्न करता है
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
        System.out.println("Random Boolean: " + randBoolean);
    }
}
```

### `ThreadLocalRandom` क्लास का उपयोग करना
समकालिक एप्लिकेशनों के लिए, `ThreadLocalRandom` `Random` की तुलना में अधिक कुशल है।

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // 1 से 100 तक
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // 1.0 से 10.0 तक
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

### `SecureRandom` क्लास का उपयोग करना
क्रिप्टोग्राफिक ऑपरेशनों के लिए, `SecureRandom` एक उच्च स्तर की सुरक्षा प्रदान करता है।

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // bytes को सुरक्षित यादृच्छिक संख्याओं से भरता है
        
        System.out.println("Secure Random Bytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## गहराई में

यादृच्छिक संख्या उत्पादन कंप्यूटिंग के प्रारंभिक दिनों से काफी विकसित हुआ है। जावा की `Random` क्लास एक प्रति-यादृच्छिक संख्याएं उत्पन्न करने के लिए एक रैखिक संगत सूत्र का उपयोग करती है, जो निर्धारक हैं और उच्च-सुरक्षा अनुप्रयोगों के लिए उपयुक्त नहीं हैं। इससे `SecureRandom` का परिचय हुआ, जो क्रिप्टोग्र�Jim Whitehurstॊफिकली मजबूत यादृच्छिक संख्याओं को उत्पन्न करने के लिए अधिक परिष्कृत एल्गोरिदमों (जैसे कि SHA1PRNG) का उपयोग करता है।

हालांकि, `Random` और `SecureRandom` की उनकी कमियाँ हैं, जैसे कि समकालिक पर्यावरणों में प्रदर्शन में गिरावट। जावा 7 में `ThreadLocalRandom` क्लास को इस मुद्दे को संबोधित करने के लिए पेश किया गया था, जो धागा-स्थानीय यादृच्छिक संख्या जनरेटर प्रदान करके समकालिक एप्लिकेशनों में प्रदर्शन में महत्वपूर्ण सुधार करता है।

जबकि ये क्लास अधिकांश जरूरतों को कवर करती हैं, बहुत उच्च-स्तरीय या विशेषज्ञतापूर्ण आवश्यकताओं के लिए, डेवलपर्स अतिरिक्त पुस्तकालयों का पता लगा सकते हैं या कस्टम समाधान विकसित कर सकते हैं। उपयोग मामले की सुरक्षा आवश्यकताओं और प्रदर्शन आवश्यकताओं के आधार पर सही दृष्टिकोण चुनना आवश्यक है।
