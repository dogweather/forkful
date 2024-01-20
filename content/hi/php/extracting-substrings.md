---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

### क्या और क्यों?

सबस्ट्रिंग्स निकालना एक विस्तारक वाक्यांश को छोटे हिस्सों में बाँटने की कला है। इसका उपयोग प्रोग्रामर्स डेटा को चुनिंदा हिस्सों में पृथक्करण, विश्लेषण और प्रसंस्करण के लिए करते हैं।

### कैसे करें:

PHP में `substr` और `strpos` फ़ंक्शन का उपयोग करके आप सबस्ट्रिंग्स निकाल सकते हैं। 

```PHP
<?php
$str = "Hello, World!";
echo substr($str, 7);  // Output: World!
?>
```
इसमें शुरुआती इंडेक्स 7 है, जिसका परिणाम स्वरूप `World!` प्रदर्शित होता है।

### गहराई की जाँच:

सबस्ट्रिंग की खोज PHP में एक बहुत पुरानी कला है जिसका इतिहास भाषा की शुरुआती दशाओं से जुड़ा हुआ है। अल्टरनेटिवली, आप `strchr`, `strrchr`, `strstr`, `stristr` जैसे अन्य फ़ंक्शन्स का भी उपयोग कर सकते हैं। `substr` आपका आदान-प्रदान सबसे अच्छा शासन करने में मदद करता है, क्योंकि आपको खोजने और वापस लौटाने के लिए अद्वितीय इकाईयों पर पूरी तरह से नियंत्रण होता है। 

### अन्य संदर्भ सामग्री:

1. [PHP String Functions Manual](https://www.php.net/manual/en/ref.strings.php)
2. [Introduction to Substring in PHP](https://www.geeksforgeeks.org/php-substr-function/)