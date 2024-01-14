---
title:                "PHP: रेगुलर एक्सप्रेशन्स का उपयोग करना"
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

रेगुलर एक्सप्रेशन को इस्तेमाल करने के चलते कोई भी व्यक्ति अपने संबंधित प्रोग्रामिंग भाषा में योग्य समांतर पाठ खोजने और विशेष वाक्यों को खोजने में सुविधा प्राप्त कर सकता है।

## कैसे करें

```PHP
// सरलता के साथ रेगुलर एक्सप्रेशन का उपयोग करें
$pattern = '/hello/';
$string = 'Hello World!';
if (preg_match($pattern, $string)) {
    echo "हैलो शब्द मिला!";
} else {
    echo "हैलो शब्द नहीं मिला।";
}
```

```PHP
// रेगुलर एक्सप्रेशन का उपयोग करके वाक्यों को बदलें
$pattern = '/[a-z]+/';
$string = '123 hello 123';
$new_string = preg_replace($pattern, 'world', $string);
echo $new_string; // "123 world 123"
```

## गहराई में जाएँ

रेगुलर एक्सप्रेशन को इस्तेमाल करने के लिए आपको उदाहरण को समझने के लिए समय निकालना होगा। आप अपने ब्राउज़र से मूल्यांकन और समझदारी की समस्याओं को सुलझा सकते हैं। इसके अलावा, आप पाठ पाठ संपादक के लिए विशिष्ट एक्सटेंशन का उपयोग करके भी सीख सकते हैं।

## देखें भी

- [रेगुलर एक्सप्रेशन फंक्शन संदर्भ पृष्ठ](https://www.php.net/manual/en/book.pcre.php)
- [पीएचपी में वाक्य परिवर्तन के लिए आम प्रयोग](https://www.geeksforgeeks.org/php-preg-replace-function/)
- [पीएचपी में रेगुलर एक्सप्रेशन का उपयोग करने के लिए एक बेहतरीन दिशा-निर्देश](https://www.w3schools.com/php/php_regex.asp)