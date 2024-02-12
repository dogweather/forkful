---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases:
- /hi/java/capitalizing-a-string.md
date:                  2024-02-03T19:06:47.951533-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को कैपिटलाइज करना का अर्थ होता है स्ट्रिंग के प्रत्येक शब्द के पहले अक्षर को अपरकेस (बड़ा अक्षर) में बदलना जबकि शेष अक्षरों को लोअरकेस (छोटे अक्षरों) में रखना। यह एक सामान्य स्ट्रिंग संशोधन कार्य है जो एप्लिकेशन में पाठ को फॉर्मेट करने के लिए उपयोगी होता है, जैसे कि उपयोगकर्ता नामों या शीर्षकों को प्रदर्शन के लिए तैयार करना कन्वेंशन या व्याकरणिक सहीपन के अनुसार।

## कैसे करें:
जावा की मानक पुस्तकालय सीधे तौर पर पूरी स्ट्रिंग्स को कैपिटलाइज करने का तरीका प्रदान नहीं करती है, लेकिन आप इसे बिल्ट-इन मेथड्स के संयोजन से हासिल कर सकते हैं। अधिक सोफिस्टिकेटेड आवश्यकताओं के लिए, थर्ड-पार्टी लाइब्रेरीज जैसे कि Apache Commons Lang सीधे समाधान प्रदान करती हैं।

### जावा के बिल्ट-इन मेथड्स का उपयोग करके
बिना बाहरी लाइब्रेरीज के स्ट्रिंग को कैपिटलाइज करने के लिए, आप स्ट्रिंग को शब्दों में विभाजित कर सकते हैं, प्रत्येक के पहले अक्षर को कैपिटलाइज कर सकते हैं, और फिर उन्हें फिर से जोड़ सकते हैं। यहाँ एक सरल दृष्टिकोण है:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // प्रिंट करेगा: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

यह कोड स्निपेट पूरी स्ट्रिंग को लोअरकेस में बदल देता है, फिर प्रत्येक अक्षर के माध्यम से इटरेट करता है, प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज करता है। यह स्पेस, पीरियड्स, और अपोस्ट्रोफ़्स को शब्द विभाजक के रूप में मानता है।

### Apache Commons Lang का उपयोग करके

Apache Commons Lang लाइब्रेरी `WordUtils.capitalizeFully()` मेथड के साथ एक अधिक सुरुचिपूर्ण समाधान प्रदान करती है, जो आपके लिए विभिन्न एज केसेस और डेलीमिटर्स को संभालती है:

```java
// निर्भरता जोड़ें: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // प्रिंट करेगा: "Hello, World!"
    }
}
```

इस मेथड का उपयोग करने के लिए, आपको अपने प्रोजेक्ट में Apache Commons Lang लाइब्रेरी को जोड़ने की आवश्यकता होगी। यह लाइब्रेरी मेथड न केवल प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज करता है, बल्कि प्रत्येक शब्द में शेष अक्षरों को लोअरकेस में भी परिवर्तित करता है, जिससे स्ट्रिंग के समूचे पैटर्न में एक सुसंगत कैपिटलाइजेशन पैटर्न सुनिश्चित होता है।
