---
title:                "एरर्स को हैंडल करना"
aliases:
- /hi/java/handling-errors.md
date:                  2024-01-26T00:53:49.330513-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/handling-errors.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एरर्स को हैंडल करने का अर्थ है कोड लिखना जो गलतियों की उम्मीद करता है और उनके साथ निबटता है। प्रोग्रामर इसे इसलिए करते हैं ताकि सॉफ्टवेयर मजबूत बन सके, जिससे क्रैशेज और अजीब व्यवहार से बचा जा सके।

## कैसे करें:

जावा एरर्स को हैंडल करने के लिए एक्सेप्शंस का उपयोग करता है। आप `try` ब्लॉक के साथ जोखिम भरे कोड को घेरते हैं और `catch` के साथ एक्सेप्शंस को पकड़ते हैं। यहाँ एक सरल उदाहरण है:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Result is: " + result);
        } catch (ArithmeticException e) {
            System.out.println("ओहो, शून्य से विभाजन नहीं कर सकते!");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

आउटपुट:
```
ओहो, शून्य से विभाजन नहीं कर सकते!
```

## गहराई से समझ:

जावा में एरर हैंडलिंग विकसित हुई है। शुरूआत के दिनों में एक्सेप्शंस नहीं होते थे; प्रोग्रामर एरर कोड्स की जाँच करते थे। फिर जावा ने try-catch ब्लॉक्स को पेश किया, जिसने एरर हैंडलिंग को अधिक सुरुचिपूर्ण बना दिया।

पारंपरिक `try-catch` के विकल्पों में `try-with-resources` शामिल है, जो स्वतः क्लोजिंग रिसोर्सेज के लिए और क्लीनर कोड के लिए, जो जावा 7 में शुरू किया गया था।

इम्पलीमेंटेशन की विवरण महत्वपूर्ण हैं। उदाहरण के लिए, `Exception` या `Throwable` को कैच करना आमतौर पर खराब प्रैक्टिस माना जाता है। यह बहुत व्यापक है, और आप जिन बग्स के बारे में जानते नहीं हो सकते हैं, उन्हें छिपा सकता है। विशिष्ट एक्सेप्शंस का पालन करें।

## यह भी देखें

- एक्सेप्शंस पर आधिकारिक ओरेकल जावा ट्यूटोरियल्स: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- जावा की `try-with-resources` स्टेटमेंट दस्तावेज़ीकरण: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- एक्सेप्शंस पर बेस्ट प्रैक्टिसेज के लिए जोशुआ ब्लोच द्वारा इफेक्टिव जावा।
