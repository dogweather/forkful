---
title:                "PHP: विंडोज़ पर लिखना:।"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्‍यों

एक उपयोगकर्ता PHP का प्रयोग करते समय कभी-कभी किसी त्रुटि का सामना करते हैं जो उनके कोड को रद्द कर देती है। इस समस्या का समाधान है "स्टैंडर्ड त्रुटि"। इसे प्रणाली की प्रकृति के अंतिम उपयोगकर्ता या विकासक दोनों को दिखाने के लिए उपयोग किया जा सकता है।

## किस प्रकार करें

जब भी हम PHP प्रोग्रामिंग में कोई त्रुटि होती है, हम इसे स्टैंडर्ड त्रुटि में लिखते हैं। ऐसा करने से हमें त्रुटि का अनुमान लगाने में और समस्या को हल करने में सहायता मिलती है।

```PHP
<?php
$text = "Hello World!";
fwrite(STDERR, $text);
?>
```

इस कोड का उत्पादन निम्नानुसार होगा:

```
Hello World!
```

## गहराई में जाना

इस तकनीक को समझने के लिए, हमें स्टैंडर्ड त्रुटि को कैसे लिखा जाए और उसका क्या उपयोग हो सकता है इसके साथ-साथ इसके लिए कॉन्फिगरेशन और उपयोग की भी जानकारी होनी चाहिए। यह तकनीक न केवल त्रुटि समाधान के लिए उपयोगी है, बल्कि कोड की स्थानीयगता और स्थानांतरण में भी मदद करती है।

## देखें भी

- [PHP में fwrite (STDERR, $ text) का उपयोग करना](https://www.w3schools.com/php/func_filesystem_fwrite.asp)
- [PHP में स्टैंडर्ड त्रुटि का उपयोग कैसे करें](https://www.geeksforgeeks.org/how-to-write-to-standard-error-in-php/)
- [PHP में स्टैंडर्ड त्रुटि क्यों लिखना चाहिए](https://stackoverflow.com/questions/20011666/why-should-i-write-to-standard-error-in-php)