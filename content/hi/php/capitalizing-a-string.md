---
title:                "PHP: स्ट्रिंग का उच्चस्वर करना"
simple_title:         "स्ट्रिंग का उच्चस्वर करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों
क्या आप PHP में किसी भी स्ट्रिंग को capitalizing करने के लिए दुबारा लिखने से परेशान हो गए हैं? उस स्तर पर आप इस ब्नॅग्लॉव के साथ णर्पणा एक कंट्रोल बनाने या स्ट्रिंग को capital करने के आसान तरीके को समझ सकते हैं।

## कैसे करें
```
<?php

$str = "hello world!";
echo strtoupper($str); // Output: HELLO WORLD!
echo ucfirst($str); // Output: Hello world!
echo ucwords($str); // Output: Hello World!
?>
```

## गहराई में जाएं
स्ट्रिंग को capital करना एक आम समस्या है और इसे हल करने के लिए कई तरीके हैं। प्रोग्रामिंग भाषाओं में इसे एक सबसे आसान तरीका है।

एक स्ट्रिंग को capital करने के लिए, सबसे पहले हम `strtoupper ()` फंक्शन का उपयोग कर सकते हैं जो सबसे आसान है। इसके बाद, हम `ucfirst ()` और `ucwords()` फंक्शन का इस्तेमाल कर सकते हैं। `ucfirst ()` फंक्शन सिर्फ पहले शब्द को capital करता है जबकि `ucwords ()` फंक्शन सभी शब्दों को capital करता है।

## देखें भी
1. [PHP Strings](http://php.net/manual/en/language.types.string.php)
2. [strtoupper() function](https://www.php.net/manual/en/function.strtoupper.php)
3. [ucfirst() function](https://www.php.net/manual/en/function.ucfirst.php)
4. [ucwords() function](https://www.php.net/manual/en/function.ucwords.php)