---
title:                "PHP: पैटर्न से मेल खाते वर्ण हटाना"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे पास ज़रूरत होती है किसी विशेष पैटर्न के अनुसार अक्षरों को हटाने की। इस ब्लॉग पोस्ट में, हम यही समझेंगे कि ऐसा इसलिए होता है और कैसे हम इसे PHP में कोडिंग कर सकते हैं।

## कैसे करें

```PHP
// सरल उदाहरण
$str = "Hello World!";
echo preg_replace("/l+/", "", $str); // आउटपुट: Heo Word!

// पूर्ण कोड
$str = "Hello World!";
$pattern = "/l+/";
$replacement = "";
echo preg_replace($pattern, $replacement, $str); // आउटपुट: Heo Word!
```

## गहरी खुराक

अब हम गहराई से जानेंगे कि ऐसा कैसे काम करता है। PHP में "preg_replace()" फ़ंक्शन का उपयोग अक्षरों के वर्णमाला में दिए गए पैटर्न के आधार पर एक्सटेंशन करने के लिए किया जाता है। इसमें हम पैटर्न, उसकी जगह पर दिए गए रिप्लेसमेंट स्ट्रिंग को इस्तेमाल करते हैं। हम इस तरह से जब भी चाहेंगे अक्षरों को हटा सकते हैं।

## देखें भी

- [PHP Regular Expressions](https://www.w3schools.com/php/php_regex.asp)
- [PHP preg_replace() function](https://www.w3schools.com/php/func_regex_preg_replace.asp)