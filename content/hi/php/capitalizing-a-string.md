---
title:                "PHP: स्ट्रिंग को बड़ी अक्षरों में लिखना"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी व्यक्ति स्ट्रिंग को कैपिटलाइज करने के लिए क्यों उपयोग करे।

## कैसे करें

कोडिंग उदाहरण और " ```PHP ... ``` " कोड ब्लॉक के भीतर आउटपुट की दृश्यता।

```PHP
$str = "hello world";
echo strtoupper($str);
```

आउटपुट: HELLO WORLD

```PHP
$str = "namaste";
echo ucfirst($str);
```

आउटपुट: Namaste

## गहराई तक जाएं

स्ट्रिंग को कैपिटलाइज करने के लिए आप पिछले दो फ़ंक्शन का उपयोग कर सकते हैं - strtoupper() और ucfirst()। strtoupper() फ़ंक्शन स्ट्रिंग में सभी छोटे अक्षरों को बड़े में बदल देता है जबकि ucfirst() फ़ंक्शन स्ट्रिंग के पहले अक्षर को बड़े में बदलता है। आप इन फ़ंक्शन का उपयोग अपने प्रोग्राम में अलग-अलग स्थितियों में कर सकते हैं, जैसे कि उपयोगकर्ता के संदेशों को कैपिटलाइज करने के लिए या पासवर्ड को बड़े में बदलने के लिए।

## इसे भी देखें

- PHP strtoupper Function: https://www.php.net/manual/en/function.strtoupper.php
- PHP ucfirst Function: https://www.php.net/manual/en/function.ucfirst.php