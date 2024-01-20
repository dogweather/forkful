---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Java में string को capitalize करना मतलब हर शब्द के पहले अक्षर को बड़ा (uppercase) बनाना है। अक्सर लोगों के नाम, जगहों की पहचान और वाक्य की शुरुआत में इसे इस्तेमाल किया जाता है।

## How to: (कैसे करें:)
```java
public class CapitalizeString {

    public static void main(String[] args) {
        String message = "यह एक उदाहरण है।";
        String capitalizedMessage = capitalizeString(message);
        System.out.println(capitalizedMessage); // Output: "यह एक उदाहरण है।" (Hindi doesn't have uppercase)
    }

    private static String capitalizeString(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        String[] words = str.split("\\s");
        StringBuilder capitalizedStr = new StringBuilder();

        for (String word : words) {
            String firstLetter = word.substring(0, 1).toUpperCase();
            String remainingLetters = word.substring(1);
            capitalizedStr.append(firstLetter).append(remainingLetters).append(" ");
        }

        return capitalizedStr.toString().trim();
    }
}
```

## Deep Dive (गहराई से समझिए):
स्ट्रिंग को capitalize करने का चलन टाइपराइटिंग के दिनों से है जब शब्दों को महत्व देने के लिए अक्षरों को बड़ा छपा जाता था। Java में String को capitalize करने के लिए `toUpperCase()` मेथड का इस्तेमाल होता है, जो कि एक शब्द या संपूर्ण स्ट्रिंग के अक्षरों को बड़ा कर देता है। 

वैकल्पिक लाइब्रेरीज जैसे Apache Commons Lang में `StringUtils.capitalize()` मेथड भी है जो यह काम करता है। लेकिन, हमेशा अतिरिक्त लाइब्रेरी जोड़ने की जरूरत नहीं पड़ती। Java में हम split(), toUpperCase(), और लूप्स के जरिये भी काम चला सकते हैं, जैसा के ऊपर कोड में किया गया है।

## See Also (और जानकारी के लिए):
- Java String Documentation: [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- Apache Commons Lang StringUtils: [https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- StackOverflow Java Capitalize First Letter: [https://stackoverflow.com/questions/3904579/how-to-capitalize-the-first-letter-of-a-string-in-java](https://stackoverflow.com/questions/3904579/how-to-capitalize-the-first-letter-of-a-string-in-java)