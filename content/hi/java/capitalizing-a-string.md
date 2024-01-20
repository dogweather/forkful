---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Java: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
जब हम एक स्ट्रिंग में दिए गए सभी अक्षर को बड़े अक्षर (कैपिटल लेटर्स) में बदलते हैं, इसे "कैपिटलाइज़" कहा जाता है। ये कुछ विशेष मामलों में उपयोगी है, जैसे की प्रोग्राम के आउटपुट में एकजुटता लाने के लिये या जब आपके पास डाटा के विभिन्न स्रोत हैं, जिनमें से कुछ उच्चकेस (uppercase) में हो सकते हैं, और कुछ लोवरकेस में।

## कैसे करें: (How to:)
ये Java के बदलाव हैं, जिनकी मदद से आप स्ट्रिंग को कैपिटलाइज़ कर सकते हैं। सबसे सरल तरीका `toUpperCase()` मेथड का उपयोग है:

```Java
String str = "hello, world!";
str = str.toUpperCase();
System.out.println(str);
```

आउटपुट:

```Java
HELLO, WORLD!
```

## गहरी जांच (Deep Dive):
कैपिटलाइज़ करने की सीधी व्याख्या "किसी शब्द के पहले अक्षर को बड़ा करने" की है। इतिहास में, यह विशेष रूप से टाइपराइडर के दिनों से है, जब लोगों को कुछ विशेष शब्दों को बड़ा करने का अवसर मिलता था।

विकल्प के रूप में, `Apache Commons Lang` लाइब्ररी का `WordUtils.capitalize()` भी उपयोग कर सकते हैं। विशेष रूप से, यदि आपको अपने स्ट्रिंग में हर शब्द के पहले अक्षर को कैपिटलाइज़ करना हो, तो यह एक बेहतर विकल्प साबित हो सकता है:

```Java
String str = "hello, world!";
str = WordUtils.capitalize(str);
System.out.println(str);
```

आउटपुट:

```Java
Hello, World!
```

## देखें भी: (See Also:)
- [Java String तरीके (Methods)](https://www.w3schools.com/java/java_ref_string.asp)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)