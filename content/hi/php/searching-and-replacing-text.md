---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट की खोज और बदलाव एक आम कार्य होता है जिसमें एक विशेष शब्द या वाक्यांश के साथ एक टेक्स्ट की खोज की जाती है और फिर उसे दूसरे टेक्स्ट से बदला जाता है। प्रोग्रामर्स इसे कार्यक्रम कॉड में गड़बड़ियां ठीक करने, कोड को काम कराने और स्थितियों का सामना करने के लिए करते हैं।

## कैसे करें:

PHP में खोज और बदलाव के लिए `str_replace()` function का उपयोग करते हैं। 

```PHP
<?php
  $string = 'नमस्ते, दुनिया!';
  $newString = str_replace('नमस्ते', 'अलविदा', $string);
  echo $newString;
?>
```

आउटपुट देखने के लिए:

```
अलविदा, दुनिया!
```

## गहन अध्ययन:

**ऐतिहासिक संदर्भ:** खोज और बदलाव केवल PHP में ही सीमित नहीं है, यह सभी प्रोग्रामिंग भाषाओं में पायी जाती है। इसका इस्तेमाल डेटा माइनिंग, मालवेयर डिटेक्टिव, और त्रुटी डिबगिंग में किया जाता है।

**विकल्प:** PHP में `preg_replace()` function भी उपलब्ध है, जो regular expressions के साथ काम करने के लिए अधिक सुविधाजनक हो सकता है।

**कार्यान्वयन विवरण:** `str_replace()` function PHP के कोर में लिखी गई है। इसमें byte-by-byte comparison का उपयोग करके मूल शब्द को विफल किया जाता है और उसके जगह नया शब्द डाला जाता है।

## और भी देखें:

अधिक जानकारी के लिए, PHP के और फंक्शनों और निर्देशों के बारे में पढ़ने के लिए निम्नलिखित स्रोतों पर जाएं:

1. [PHP str_replace](https://www.php.net/manual/en/function.str-replace.php)
2. [PHP preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
3. [Regular Expressions in PHP](https://www.php.net/manual/en/book.pcre.php)