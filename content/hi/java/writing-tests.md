---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेस्टिंग का मतलब है अपने कोड की जांच करना ये सुनिश्चित करने के लिए कि वह सही तरीके से काम कर रहा है। प्रोग्रामर इसलिए टेस्ट लिखते हैं ताकि बग्स को जल्दी पकड़ सकें और सॉफ्टवेयर की गुणवत्ता बढ़ा सकें।

## कैसे करें:
```java
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}

public class CalculatorTest {
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3));
    }
}
```
सैंपल आउटपुट: अगर टेस्ट पास होता है तो कोई आउटपुट नहीं होगा। अगर फेल होता है, तो एक एरर मैसेज दिखाई देगा।

## गहराई से जानकारी:
जूनिट जैसे फ्रेमवर्क्स 90 के दशक से जावा में टेस्टिंग को सरल बना रहे हैं। टेस्टिंग के अन्य तरीके में TestNG शामिल है। टेस्ट केसेज लिखते समय, ध्यान देना होता है की कोड की सभी शाखाओं को कवर किया जाए।

## इससे भी जानें:
- जूनिट ऑफिशियल डॉक्यूमेंटेशन: https://junit.org/junit5/docs/current/user-guide/
- TestNG ऑफिशियल वेबसाइट: https://testng.org/doc/
- मार्टिन फाउलर के ब्लॉग पर टेस्टिंग का अवलोकन: https://martinfowler.com/articles/practical-test-pyramid.html
- बाउंड्री टेस्टिंग के लिए कम्प्लीट गाइड: https://www.guru99.com/boundary-value-analysis.html
