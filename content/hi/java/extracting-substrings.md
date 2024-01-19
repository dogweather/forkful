---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# जावा में उपवाक्यांश निकालना

## क्या और क्यों?

उपवाक्यांश निकालना एक वाक्यांश या स्ट्रिंग निकालने की एक क्रिया है। प्रोग्रामर इसे डेटा को छोटे खंडों में विभाजित करने और प्रबंधित करने के लिए करते हैं।

## कैसे करें:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, world!";
        String substr = str.substring(7, 12);
        System.out.println(substr);  // Outputs "world"
    }
}
```
इस कोड का उदाहरण फ़ंक्शन `substring(7, 12)` का उपयोग करके `"Hello, world!"` वाक्यांश से `"world"` उपवाक्यांश को निकालता है।

## बारीकी से समझना

जावा भाषा में `substring` क्रिया का प्रयोग 1990 वीं सताब्दी में हुआ था। इसके विकल्पों में `split`, `indexOf` और `charAt` शामिल हैं, परन्तु `substring` बहुत ही लोकप्रिय हुआ। इसे जब कोई वाक्यांश छोटे खण्डों में विभाजित करना होता है तो इसका उपयोग किया जाता है।

`substring` क्रिया अंत सूचक को छोड़कर सभी वर्णों को शामिल करती है। यदि आपके पास एक स्ट्रिंग `"Hello, world!"` है और आप `substring(0, 5)` का उपयोग करते हैं, तो आपको `"Hello"` मिलेगा।

## जोर देखें

1. [Java String substring() Method](https://www.javatpoint.com/java-string-substring)
2. [Java - String substring() Method](https://www.tutorialspoint.com/java/java_string_substring.htm)