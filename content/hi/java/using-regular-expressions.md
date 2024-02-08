---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- hi/java/using-regular-expressions.md
date:                  2024-02-03T19:18:07.759499-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

जावा में नियमित अभिव्यक्तियाँ (regex) आपको अपने कोड में स्ट्रिंग्स को खोजने, संशोधित करने या सत्यापित करने के लिए विशिष्ट पैटर्न निर्धारित करने की अनुमति देती हैं। प्रोग्रामर लॉग फाइलों को पार्स करने, उपयोगकर्ता इनपुट को मान्य करने, या पाठ के भीतर विशिष्ट पैटर्न की खोज करने जैसे कार्यों के लिए उनका उपयोग करते हैं, यह न्यूनतम प्रयास के साथ जटिल स्ट्रिंग प्रोसेसिंग को सक्षम करता है।

## कैसे:

जावा में रेगेक्स के लिए निर्मित सहायता मुख्य रूप से `Pattern` और `Matcher` कक्षाओं के माध्यम से होती है जो इस `java.util.regex` पैकेज में होती है। यहाँ एक साधारण उदाहरण दिया गया है जो एक स्ट्रिंग में एक शब्द की सभी घटनाओं को खोजने और मुद्रित करने के लिए है, केस असंवेदनशील:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Found '" + matcher.group() + "' at position " + matcher.start());
        }
    }
}
```

आउटपुट:
```
Found 'parsing' at position 16
Found 'Parsing' at position 31
```

स्ट्रिंग्स को विभाजित करने जैसे कार्यों के लिए, आप `String` कक्षा के `split()` विधि को एक regex के साथ उपयोग कर सकते हैं:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

आउटपुट:
```
Java
Python
Ruby
JavaScript
```

जावा में regex के साथ काम करते समय, हो सकता है कि कुछ जटिल कार्यों को सरल बनाने के लिए कोई बाहरी पुस्तकालय उपयोगी हो। जावा में regex के साथ काम करने के लिए लोकप्रिय तीसरे पक्ष की पुस्तकालयों में से एक है `Apache Commons Lang`। इसमें `StringUtils` जैसे उपयोगिताओं की पेशकश की जाती है जो कुछ regex कार्यों को अधिक सरल बनाती है। यहाँ इसका उपयोग करने का तरीका दिया गया है एक सबस्ट्रिंग के मिलान की संख्या गिनने के लिए:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' appears " + count + " times.");
    }
}
```

Apache Commons Lang को उपयोग करने के लिए, आपको इसे अपने प्रोजेक्ट में शामिल करना होगा। यदि आप Maven का उपयोग कर रहे हैं, तो अपने `pom.xml` में यह निर्भरता जोड़ें:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- नवीनतम संस्करण के लिए जांचें -->
</dependency>
```

आउटपुट:
```
'processing' appears 2 times.
```
