---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

खोजना और प्रतिस्थापना, यानी searching और replacing, सामग्री के विशिष्ट हिस्सों को खोजने और उन्हें नई सामग्री से प्रतिस्थापित करने की प्रक्रिया है। प्रोग्रामर्स इसे कोड पर वर्क फ्लो को अधिक कुशल बनाने के लिए करते हैं। 

## कैसे?

आइए देखते हैं कि Java में मूल स्ट्रिंग में किसी विशेष शब्द को खोजकर उसे अन्य शब्द से कैसे बदला जा सकता है।

```Java
public class Main {
  public static void main(String[] args) {
    String s = "नमस्ते दुनिया!";
    String r = s.replace("नमस्ते", "अलविदा");
    System.out.println(r);
  }
}
```
आउटपुट:

```Java
अलविदा दुनिया!
```

## गहराई का अध्ययन

1. ऐतिहासिक सन्दर्भ: Java 1.0 से ही, `replace` मेथड Java की String क्लास में मौजूद है। इसकी मुख्य वजह यह है कि यह एक आम कार्य है जो बार-बार उपयोग किया जाता है। 

2. विकल्प: `replace` का उपयोग करने के अलावा, `replaceAll` और `replaceFirst` मेथड्स भी मौजूद हैं जो अधिक विशेष cases के लिए उपयोगी हो सकते हैं।

3. कार्यान्वयन विवरण: इन मेथड्स का कार्यान्वयन आमतौर पर regex या regex पैटर्न्स का उपयोग करके किया जाता है। 

## अधिक जानकारी के लिए

* Java 8 Documentation: String (https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
* Stack Overflow: How to replace a character in a string in Java? (https://stackoverflow.com/questions/16027229/how-to-replace-a-character-in-a-string-in-java)
* Geeks for Geeks: Replace a character in a String in Java (https://www.geeksforgeeks.org/replace-a-character-in-a-string-in-java)