---
title:                "PHP: स्ट्रिंग को लोअर केस में बदलना"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपनी कोडिंग में स्ट्रिंग को lowercase में बदलने की जरूरत होती है। इससे पुराने और नये स्ट्रिंग के बीच में सही संबंध बनाए रखने में मदद मिलती है। इस ब्लॉग पोस्ट में हम इस विषय पर गहराई से जाएंगे।

## कैसे करें

इस काम को करने के लिए हमको निम्न शब्दों का इस्तेमाल करना होगा:

```PHP
<?php
$str = "Hello World";
echo strtolower($str);
?>
```

उपरोक्त कोड का आउटपुट "hello world" होगा। यहां `strtolower()` एक फ़ंक्शन है जो दिए गए स्ट्रिंग को lowercase में बदलता है। इसी तरह हम `strtoupper()` फ़ंक्शन का भी इस्तेमाल करके स्ट्रिंग को uppercase में बदल सकते हैं।

## गहराई से जाने

स्ट्रिंग को lowercase में बदलने के पीछे का मूल उद्देश्य स्पष्ट है - सही रूप से संबंध बनाना। इससे स्ट्रिंग की प्रकार या बैदारिक भाषा को जानने की जरूरत नहीं होती है। यदि कोडिंग में strict स्ट्रिंग संबंध बनाना हो तो सभी स्ट्रिंग को lowercase या uppercase में बदल देना एक अच्छा तरीका है।

## और जानें

अगर आपको इस ब्लॉग पोस्ट के बारे में और जानना है तो आप निम्नलिखित लिंक्स को देख सकते हैं:

- `strtolower()` फ़ंक्शन की PHP ऑफ़िशियल डॉक्यूमेंटेशन: https://www.php.net/manual/en/function.strtolower.php
- `strtoupper()` फ़ंक्शन की PHP ऑफ़िशियल डॉक्यूमेंटेशन: https://www.php.net/manual/en/function.strtoupper.php