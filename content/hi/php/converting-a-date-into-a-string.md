---
title:                "PHP: तारीख को स्ट्रिंग में बदलना"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप PHP में तारीख को वस्तु स्ट्रिंग में बदलना चाहते हैं तो आप इसका उपयोग किस लिए कर सकते हैं?

## कैसे करें
```PHP
// हम एक उदाहरण तारीख ले और इसे वस्तु स्ट्रिंग में बदलेंगे
$date = date_create("2021-09-15");

// तारीख को वस्तु स्ट्रिंग में बदलने के लिए date_format() फ़ंक्शन का उपयोग करें
$date_string = date_format($date, "d-m-Y");

// निर्दिष्ट प्रारूप के अनुसार वस्तु स्ट्रिंग को प्रिंट करें
echo $date_string;
```

आउटपुट:
```
15-09-2021
```

## गहराई में जाएं
तारीख को वस्तु स्ट्रिंग में बदलना एक आसान कार्य ऊपर दिए गए उदाहरण से साधित किया जा सकता है। date_create() और date_format() फ़ंक्शन हमें इस काम में मदद करते हैं। इसके अलावा, हम समझ सकते हैं कि वस्तु स्ट्रिंग का प्रारूप कैसे निर्धारित किया जा सकता है और इसमें उपयोगी उपकरण हमें कौन से मिलते हैं।

## देखें भी
- [PHP date_create() फ़ंक्शन की डॉक्यूमेंटेशन](https://www.php.net/manual/en/function.date-create.php)
- [PHP date_format() फ़ंक्शन की डॉक्यूमेंटेशन](https://www.php.net/manual/en/function.date-format.php)
- [वस्तु स्ट्रिंग का परिभाषा और इसे लिखने का समझ](https://www.tutorialspoint.com/php/php_strings.htm)