---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Strings को मिलाना (concatenate) का मतलब होता है दो या दो से अधिक strings को एक साथ जोड़ना। Programmers को आमतौर पर एक से ज्यादा strings को combine करने, form messages या display output में use करने के लिए इसकी जरूरत होती है।


## कैसे करें:
आप नीचे दिए गए Java कोड का उपयोग करके strings को concatenate कर सकते हैं। 

```Java
public class Main {

    public static void main(String[] args) {
        String string1 = "नमस्ते, ";
        String string2 = "दुनिया!";
        String string3 = string1 + string2;
        System.out.println(string3);
    }
}
```

ऊपरी कोड का output होगा:
```Java
नमस्ते, दुनिया!
```

## गहरी जानकारी:
(१) ऐतिहासिक प्रसंग: जब Java पहले बनाया गया था तो String concatenation का मुख्य तरीका `+` operator का उपयोग था।

(२) विकल्प: `StringBuilder` और `StringBuffer` क्लास का उपयोग कर सकते हैं जो बहुत बड़े strings के साथ काम करने के लिए अधिक कुशल होते हैं।

(३) कार्यान्वयन विवरण: Strings immutable होते हैं जिसका मतलब है कि एक बार बनाने के बाद उन्हें modify नहीं किया जा सकता। जब आप strings को concatenate करते हैं, तो Java नयी string बनाता है।

## आगे देखें:
आप [Oracle की Java डॉक्युमेंटेशन](https://docs.oracle.com/en/java/javase/13/docs/api/index.html) में String और [StringBuilder](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html) class की अधिक जानकारी पा सकते हैं।