---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:47.951533-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u093E\u0935\
  \u093E \u0915\u0940 \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0938\u0940\u0927\u0947 \u0924\u094C\u0930 \u092A\u0930 \u092A\
  \u0942\u0930\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938\
  \ \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\
  \u0928\u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\u0940 \u0939\u0948, \u0932\
  \u0947\u0915\u093F\u0928 \u0906\u092A \u0907\u0938\u0947 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u092E\u0947\u0925\u0921\u094D\u0938 \u0915\u0947 \u0938\u0902\
  \u092F\u094B\u091C\u0928 \u0938\u0947\u2026"
lastmod: '2024-04-05T21:53:54.104939-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u093E\u0935\u093E \u0915\u0940 \u092E\u093E\u0928\u0915 \u092A\u0941\
  \u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0938\u0940\u0927\u0947 \u0924\u094C\
  \u0930 \u092A\u0930 \u092A\u0942\u0930\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917\u094D\u0938 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\
  \u0907\u091C \u0915\u0930\u0928\u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E\
  \ \u092A\u094D\u0930\u0926\u093E\u0928 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\
  \u0940 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0906\u092A \u0907\u0938\u0947\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092E\u0947\u0925\u0921\u094D\u0938\
  \ \u0915\u0947 \u0938\u0902\u092F\u094B\u091C\u0928 \u0938\u0947 \u0939\u093E\u0938\
  \u093F\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0905\
  \u0927\u093F\u0915 \u0938\u094B\u092B\u093F\u0938\u094D\u091F\u093F\u0915\u0947\u091F\
  \u0947\u0921 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E\u0913\u0902 \u0915\
  \u0947 \u0932\u093F\u090F, \u0925\u0930\u094D\u0921-\u092A\u093E\u0930\u094D\u091F\
  \u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u091C\u0948\
  \u0938\u0947 \u0915\u093F Apache Commons Lang \u0938\u0940\u0927\u0947 \u0938\u092E\
  \u093E\u0927\u093E\u0928 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\
  \u0940 \u0939\u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

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
