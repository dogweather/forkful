---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन टेक्स्ट पैटर्न की खोज और मैनिपुलेशन के लिए एक तकनीक है. प्रोग्रामर्स इसका इस्तेमाल जटिल टेक्स्ट डाटा को जल्दी और सटीकता से प्रोसेस करने के लिए करते हैं.

## How to: (कैसे करें:)
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        // उपयोग करने के लिए पैटर्न बनाएं
        Pattern pattern = Pattern.compile("फ[अ-औ]ल");

        // टेक्स्ट से मैच ढूंढें
        Matcher matcher = pattern.matcher("फल, फिल, फुल, फेल, फॉल");

        // मैच करने वाले सभी शब्दों को प्रिंट करें
        while(matcher.find()) {
            System.out.println("मिला हुआ शब्द: " + matcher.group());
        }
    }
}
```
सैंपल आउटपुट:
```
मिला हुआ शब्द: फल
मिला हुआ शब्द: फिल
मिला हुआ शब्द: फुल
मिला हुआ शब्द: फॉल
```

## Deep Dive (गहराई में जानकारी)
रेगुलर एक्सप्रेशन का इतिहास 1950 के दशक तक जाता है. एल्गोरिदम और थ्योरी कंप्यूटिंग में इसका महत्व है. विकल्पों में स्ट्रिंग मैथड्स (indexOf, subString) और थर्ड-पार्टी लाइब्रेरीज़ (Apache Commons, Google Guava) शामिल हैं. जावा में, `Pattern` और `Matcher` क्लास जावा यूटिल रेगेक्स पैकेज में इस्तेमाल होते हैं.

## See Also (और जानकारी के लिए)
- Oracle Java Docs: https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html
- Regular-Expressions.info: https://www.regular-expressions.info/
- JavaWorld article on Java regex: https://www.javaworld.com/category/regular-expressions/
